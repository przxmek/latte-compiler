@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@lf  = internal constant [4 x i8] c"%lf\00"

%FILE = type opaque
@stdin = external global %FILE*

declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @malloc(i32)
declare void @free(i8*)
declare i8* @strcat(i8*, i8*)
declare i8* @strcpy(i8*, i8*)
declare i32 @strlen(i8*)
declare i32 @getline(i8**, i32*, %FILE*)


define void @printInt(i32 %x) {
entry: %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
	ret void
}

define void @printDouble(double %x) {
entry: %t0 = getelementptr [6 x i8], [6 x i8]* @fnl, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %t0, double %x)
	ret void
}

define void @printString(i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

define i32 @readInt() {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
	%t2 = load i32, i32* %res
	ret i32 %t2
}

define double @readDouble() {
entry:	%res = alloca double
        %t1 = getelementptr [4 x i8], [4 x i8]* @lf, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, double* %res)
	%t2 = load double, double* %res
	ret double %t2
}

define i8* @readString() {
	%buff_ptr = alloca i8*
	store i8* null, i8** %buff_ptr
	%buff_size = alloca i32
	store i32 1024, i32* %buff_size
	%in = load %FILE*, %FILE** @stdin
	%size = call i32 @getline(i8** %buff_ptr, i32* %buff_size, %FILE* %in)
	%cond = icmp sle i32 %size, 1
	br i1 %cond, label %EMPTY_LINE, label %OK
EMPTY_LINE:
	%new_res = call i8* @readString()
	ret i8* %new_res
OK:
	%index = sub i32 %size, 1
	%buff = load i8*, i8** %buff_ptr
	%last = getelementptr i8, i8* %buff, i32 %index
	store i8 0, i8* %last
	ret i8* %buff
}

define i8* @concat(i8* %a, i8* %b) {
	%alen = call i32 @strlen(i8* %a)
	%blen = call i32 @strlen(i8* %b)
	%ablen = add i32 %alen, %blen
	%len = add i32 %ablen, 1
	%res = call i8* @malloc(i32 %len)
	call i8* @strcpy(i8* %res, i8* %a)
	call i8* @strcat(i8* %res, i8* %b)
	ret i8* %res
}
