<dfForm action="/">
    <dfChildErrorList ref="" />

    <dfLabel ref="name">Name: </dfLabel>
    <dfInputText ref="name" />
    <br>

    <dfLabel ref="password">Password: </dfLabel>
    <dfInputPassword ref="password" />
    <br>

    Birthday:
    <dfSubView ref="birthdate">
        <apply template="date-form" />
    </dfSubView> 
    <br>

    <dfInputSubmit value="Enter" />
</dfForm>
