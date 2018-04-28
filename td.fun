x = fun {
    print y
    y = y + 1

    x = fun print 5732
}

y = 1000

x()

print y

x()
