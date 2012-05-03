<dfForm action="/">
    <dfChildErrorList />

    <dfLabel ref="name">Name: </dfLabel>
    <dfInputText ref="name" />
    <br>

    <dfLabel ref="password">Password: </dfLabel>
    <dfInputPassword ref="password" />
    <br>

    <dfLabel ref="sex">Sex: </dfLabel>
    <dfInputSelect ref="sex" />
    <br>

    Birthday:
    <dfSubView ref="birthdate">
        <apply template="date-form" />
    </dfSubView> 
    <br>

    <dfInputSubmit value="Enter" />
</dfForm>
