@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@lf  = internal constant [4 x i8] c"%lf\00"

%FILE = type opaque
declare %FILE* @fdopen(i32, i8*)
@r = constant [2 x i8] c"r\00"

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
	%buff = call i8* @malloc(i32 1024)
	%buff_ptr = alloca i8*
	store i8* %buff, i8** %buff_ptr
	%flag = getelementptr inbounds [2 x i8], [2 x i8]* @r, i32 0, i32 0
	%stdin = call %FILE* @fdopen(i32 0, i8* %flag)
	%buff_size = alloca i32
	store i32 1024, i32* %buff_size
	%size = call i32 @getline(i8** %buff_ptr, i32* %buff_size, %FILE* %stdin)
	%index = sub i32 %size, 1
	%last = getelementptr i8, i8* %buff, i32 %index
	store i8 9, i8* %last
	%res = call i8* @malloc(i32 %size)
	call i8* @strcpy(i8* %res, i8* %buff)
	call void @free(i8* %buff)
	ret i8* %res
}

define i8* @concat(i8* %a, i8* %b) {
	%t1 = call i32 @strlen(i8* %a)
	%t2 = call i32 @strlen(i8* %b)
	%t3 = add i32 %t1, %t2
	%t4 = add i32 %t3, 1
	%t5 = call i8* @malloc(i32 %t4)
	call i8* @strcpy(i8* %t5, i8* %a)
	call i8* @strcat(i8* %t5, i8* %b)
	ret i8* %t5
}
