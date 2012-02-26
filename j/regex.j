include("pcre.j")

## object-oriented Regex interface ##

type Regex
    pattern::ByteString
    options::Int32
    regex::Array{Uint8}
    extra::Ptr{Void}

    function Regex(pat::String, opts::Integer, study::Bool)
        pat = cstring(pat); opts = int32(opts)
        if (opts & ~PCRE_OPTIONS_MASK) != 0
            error("invalid regex option(s)")
        end
        re = pcre_compile(pat, opts & PCRE_COMPILE_MASK)
        ex = study ? pcre_study(re) : C_NULL
        new(pat, opts, re, ex)
    end
end
Regex(p::String, s::Bool)    = Regex(p, 0, s)
Regex(p::String, o::Integer) = Regex(p, o, false)
Regex(p::String)             = Regex(p, 0, false)

# TODO: make sure thing are escaped in a way PCRE
# likes so that Julia all the Julia string quoting
# constructs are correctly handled.

macro r_str(pattern, flags...)
    options = 0
    for fx in flags, f in fx
        options |= f=='i' ? PCRE_CASELESS  :
                   f=='m' ? PCRE_MULTILINE :
                   f=='s' ? PCRE_DOTALL    :
                   f=='x' ? PCRE_EXTENDED  :
                   error("unknown regex flag: $f")
    end
    Regex(pattern, options)
end

function show(re::Regex)
    if (re.options & ~(PCRE_CASELESS|PCRE_MULTILINE|PCRE_DOTALL|PCRE_EXTENDED))==0
        print('r')
        print_quoted_literal(re.pattern)
        if (re.options & PCRE_CASELESS ) != 0; print('i'); end
        if (re.options & PCRE_MULTILINE) != 0; print('m'); end
        if (re.options & PCRE_DOTALL   ) != 0; print('s'); end
        if (re.options & PCRE_EXTENDED ) != 0; print('x'); end
    else
        print("Regex(")
        show(re.pattern)
        print(',')
        show(re.options)
        print(')')
    end
end

type RegexMatch
    match::ByteString
    captures::Tuple
    offset::Int
    offsets::Vector{Int}
end

function show(m::RegexMatch)
    print("RegexMatch(")
    show(m.match)
    if !isempty(m.captures)
        print(", ")
        for i = 1:length(m.captures)
            print(i, "=")
            show(m.captures[i])
            if i < length(m.captures)
                print(", ")
            end
        end
    end
    print(")")
end

matches(r::Regex, s::String, o::Integer) = pcre_exec(r.regex, r.extra, cstring(s), 1, o, false)
matches(r::Regex, s::String) = matches(r, s, r.options & PCRE_EXECUTE_MASK)

function match(re::Regex, str::ByteString, offset::Integer, opts::Integer)
    m, n = pcre_exec(re.regex, re.extra, str, offset, opts, true)
    if isempty(m); return nothing; end
    mat = str[m[1]+1:m[2]]
    cap = ntuple(n, i->(m[2i+1] < 0 ? nothing : str[m[2i+1]+1:m[2i+2]]))
    off = map(i->m[2i+1]+1, [1:n])
    RegexMatch(mat, cap, m[1]+1, off)
end
match(r::Regex, s::String, o::Integer, p::Integer) = match(r, cstring(s), o, p)
match(r::Regex, s::String, o::Integer) = match(r, s, o, r.options & PCRE_EXECUTE_MASK)
match(r::Regex, s::String) = match(r, s, 1)

type RegexMatchIterator
    regex::Regex
    string::ByteString
    overlap::Bool
end

start(itr::RegexMatchIterator) = match(itr.regex, itr.string)
done(itr::RegexMatchIterator, m) = m == nothing
next(itr::RegexMatchIterator, m) =
    (m, match(itr.regex, itr.string, m.offset + (itr.overlap ? 1 : length(m.match))))

each_match(r::Regex, s::String) = RegexMatchIterator(r,s,false)
each_match_overlap(r::Regex, s::String) = RegexMatchIterator(r,s,true)

function split(s::String, regex::Regex, include_empty::Bool)
    s = cstring(s)
    i = j = start(s)
    strs = typeof(s)[]
    while !done(s,i)
        m = match(regex,s,j)
        if m == nothing
            break
        end
        tok = s[i:m.offset-1]
        if include_empty || !isempty(tok)
            push(strs, tok)
        end
        i = m.offset+length(m.match)
        j = m.offset+max(1,length(m.match))
    end
    if include_empty || i < length(s)
        push(strs, s[i:end])
    end
    return strs
end

split(s::String, x::String, incl::Bool) = split(s, Regex(strcat("\\Q",x)), incl)

#Ensure something is a regex. (trusting the type calculation)
ensure_regex(obj::Regex,bool)  = obj
ensure_regex(obj::String,bool) = Regex(obj,bool)

#TODO i don't like overdocumentation in files, where will this go?
#Macro that takes a string, regular expressions and variables and
# fills the variables as such:
# * String/Regex; skip-to them, 
#   fills what is between.(match.match is discarded)
# * Symbol; this is is filled with what is between matches.
# * Array with Symbol and String/Regex, skip to it, and
#   fill the variable with the match
# * TODO possibility for further behavior, but not clear what to do.
macro regex_var (input_string, vars_and_regex, input_body)
  matching,string= gensym(2) #Will have to ask why not denote it by itself
#  @assert vars_and_regex.head == :hcat
  vars_array= vars_and_regex.args 
#If it sees the end, it stops, otherwise it continues.
  function maybe_rv(vars,body) #TODO interim here too
    if length(vars)>0 
      return rv(vars[1], nothing, vars[2:],body) 
    end
    return body
  end
#If it is specified that stuff between needs to be put in a variable, do so.
  maybe_interim(interim::Nothing, m) = nothing
  function maybe_interim(interim::Symbol, m) 
    return quote $interim = $matching ? ($string)[1:($m).offset-1] : nothing end
  end

# Invalid is not recognized.(More definitions may be added)
  function rv (first_var, interim, vars, body)
    error("Invalid input for regex-binding: $first_var 
type: $(typeof(first_var))") 
  end
# Receive a request for capturing interim.
  function rv (first_var::Symbol, interim, vars, body)
#    @assert interim == nothing #Only do an interim once.
    if length(vars) > 1
      return rv(vars[1], first_var, vars[2:],body)
    else return quote begin
      $first_var = ($string)#The remaining string is 'the interim to the end'
      $body                 # (sounds like a song name)
    end end
  end
#Skipping, possibly capturing interim.
  function rv(first_var::String, interim, vars, body)
    m= gensym()
    return quote
        $m= $matching ? 
            match(ensure_regex($first_var,false),$string,start($string)) : 
            nothing
        $matching = ($m!=nothing) #&& $matching but that is already so.
      #Everything else is nothing, but we'll only know at run-time!
        $(maybe_interim(interim, m))
        if $matching
          $string = ($string)[($m).offset + length(($m).match):]
        end
        $(maybe_rv(vars,body))
      end
    end
  end
#Binding.(possibly with interim)
  function rv(first_var::Expr,interim, vars, body)
#    @assert first_var.head == :hcat
    first_var = first_var.args
#    @assert typeof(first_var[1]) <: Symbol #Stick to form, for it may expand.
#    @assert typeof(first_var[2]) <: String || first_var[2] <: Regex 
#    @assert length(first_var) == 2
    m= gensym()
    return quote
      $m= $matching ? 
          match(ensure_regex($first_var[2],false),$string,start($string)) :
          nothing
      $matching = ($m!=nothing) #&& $matching but that is already so.
      $(maybe_interim(interim, m))
      $(first_var[1]) = ($m).match # Capture match 
      if $matching
        $string = ($string)[($m).offset + length(($m).match):]
      end
      $(maybe_rv(vars,body))
    end
  end
#And actually do the work.
  return quote
    $matching = true #Start out matching, stop at first mismatch.
    $string= $input_string
    $(rv(vars_array[1], nothing, vars_array[2:],input_body))
  end
end
