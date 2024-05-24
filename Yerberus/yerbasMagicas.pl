:- use_module(library(pce)).

%  mostrar una imagen en la interfaz
mostrar(V, D, M) :-
    new(I, image(V)),
    new(B, bitmap(I)),
    new(F2, figure),
    send(F2, display, B),
    new(D1, device),
    send(D1, display, F2),
    send(D, display, D1),
    send(D1, below(M)).

% preguntar al usuario que ingrese una planta o síntoma
preguntar_input(Prompt, Label, Input) :-
    new(P, dialog),
    send(P, kind, transient),
    send(P, append, label(prompt)),
    send(P, append, new(TI, text_item(name, '', message(P?ok_member, execute)))),
    send(P, append, button(ok, message(P, return, TI?selection))),
    send(P, append, button(cancel, message(P, return, @nil))),
    send(TI, label, Label),
    send(TI, selection, Prompt),
    get(P, confirm_centered, RawInput),
    send(P, show, @off),
    RawInput \== @nil,
    Input = RawInput.

% Lista con las plantas a tratar en la actividad
iman('ruda', 'C:/Yerberus/imgs/Ruda.jpg').
iman('ruibarbo', 'C:/Yerberus/imgs/Ruibarbo.jpg').
iman('salvia', 'C:/Yerberus/imgs/Salvia.jpg').
iman('sen', 'C:/Yerberus/imgs/Sen.jpg').

% Desplegar la información de las plantas
mostrar_planta_info(Planta) :-
    new(Dialog, dialog('Información sobre la planta')),
    send(Dialog, size, size(650, 450)),
    send(Dialog, colour, colour(white)),
    send(Dialog, append, new(Menu, menu_bar)),
    send(Dialog, display, text('Planta medicinal:', left, normal), point(350, 20)),
    send(Dialog, display, text(Planta, left, bold), point(350, 40)),
    % Obtener y mostrar el nombre científico
    nombre_cientifico(Planta, NombreCientifico),
    send(Dialog, display, text('Nombre científico:', left, normal), point(350, 60)),
    send(Dialog, display, text(NombreCientifico, left, bold), point(350, 80)),
    % Obtener y mostrar las propiedades curativas
    findall(Propiedad, propiedad_curativa(Planta, Propiedad), Propiedades),
    atomic_list_concat(Propiedades, ', ', PropiedadesText),
    send(Dialog, display, text('Propiedades curativas:', left, normal), point(350, 100)),
    send(Dialog, display, text(PropiedadesText, left, bold), point(350, 120)),
    % Obtener y mostrar el consumo recomendado
    findall(Consumo, consumo(Planta, Consumo), Consumos),
    atomic_list_concat(Consumos, ', ', ConsumosText),
    send(Dialog, display, text('Formas de consumo:', left, normal), point(350, 140)),
    send(Dialog, display, text(ConsumosText, left, bold), point(350, 160)),
    % Obtener y mostrar los usos terapéuticos
    findall(Uso, uso_terapeutico(Planta, Uso), Usos),
    atomic_list_concat(Usos, ', ', UsosText),
    send(Dialog, display, text('Usos terapéuticos:', left, normal), point(350, 180)),
    send(Dialog, display, text(UsosText, left, bold), point(350, 200)),
    % Obtener y mostrar los efectos secundarios
    findall(Efecto, efecto_secundario(Planta, Efecto), Efectos),
    atomic_list_concat(Efectos, ', ', EfectosText),
    send(Dialog, display, text('Efectos secundarios:', left, normal), point(350, 220)),
    send(Dialog, display, text(EfectosText, left, bold), point(350, 240)),
    % Obtener y mostrar las precauciones
    findall(Precaucion, precaucion(Planta, Precaucion), Precauciones),
    atomic_list_concat(Precauciones, ', ', PrecaucionesText),
    send(Dialog, display, text('Precauciones:', left, normal), point(350, 260)),
    send(Dialog, display, text(PrecaucionesText, left, bold), point(350, 280)),
    % Obtener y mostrar el origen
    origen(Planta, Origen),
    send(Dialog, display, text('Origen:', left, normal), point(350, 300)),
    send(Dialog, display, text(Origen, left, bold), point(350, 320)),
    % Obtener y mostrar la imagen de la planta
    iman(Planta, Foto),
    mostrar(Foto, Dialog, Menu),
    send(Dialog, open_centered).

% Predicado para mostrar información sobre un síntoma
mostrar_sintoma_info(Sintoma) :-
    new(Dialog, dialog('Información sobre síntoma')),
    send(Dialog, size, size(600, 500)),
    send(Dialog, append, label(nombre, 'Síntoma:')),
    send(Dialog, append, text(Sintoma)),
    send(Dialog, append, label(tratamiento, 'Tratamiento con plantas medicinales:')),
    findall(Planta, trata_enfermedad(Planta, Sintoma), Plantas),
    atomic_list_concat(Plantas, ', ', PlantasText),
    send(Dialog, append, text(PlantasText)),
    send(Dialog, open_centered).

% Predicado para mostrar información sobre una enfermedad
mostrar_enfermedad(Enfermedad) :-
    new(Dialog, dialog('Información sobre la Enfermedad')),
    send(Dialog, size, size(600, 500)),
    send(Dialog, append, label(nombre, 'Enfermedad:')),
    send(Dialog, append, text_item(Enfermedad, Enfermedad, @default)),
    send(Dialog, append, label(concepto, 'Descripción:')),
    descripcion_enfermedad(Enfermedad, Descripcion),
    send(Dialog, append, text_item(Descripcion, Descripcion, @default)),
    send(Dialog, open_centered).

mostrar_diccionario(Palabra) :-
    new(Dialog, dialog('Información sobre la Palabra')),
    send(Dialog, size, size(600, 500)),
    send(Dialog, append, label(nombre, 'Palabra:')),
    send(Dialog, append, text(Palabra)),
    send(Dialog, append, label(concepto, 'Concepto:')),
    concepto(Palabra, Concepto),
    send(Dialog, append, text(Concepto)),
    send(Dialog, open_centered).

% Mostrar datos curiosos (solo imagen)
mostrar_datos_importantes :-
    new(Dialog, dialog('Datos Importantes')),
    send(Dialog, size, size(600, 500)),
    new(Pic, picture),
    send(Dialog, append, Pic),
    send(Pic, display, new(Bitmap, bitmap('C:/Yerberus/imgs/DatosImportantes.jpg'))),
    send(Dialog, open_centered).

% Mostrar referencias (solo imagen)
mostrar_referencias :-
    new(Dialog, dialog('Referencias')),
    send(Dialog, size, size(600, 500)),
    new(Pic, picture),
    send(Dialog, append, Pic),
    send(Pic, display, new(Bitmap, bitmap('C:/Yerberus/imgs/Referencias.jpg'))),
    send(Dialog, open_centered).

% Hechos y reglas 
nombre_cientifico('ruda', 'Ruta graveolens').
nombre_cientifico('ruibarbo', 'Rheum rhabarbarum').
nombre_cientifico('salvia', 'Salvia officinalis').
nombre_cientifico('sen', 'Senna alexandrina').

propiedad_curativa('ruda', 'Antiinflamatorio').
propiedad_curativa('ruibarbo', 'Digestivo').
propiedad_curativa('salvia', 'Antiséptico').
propiedad_curativa('sen', 'Laxante').

consumo('ruda', 'Infusión').
consumo('ruibarbo', 'Tintura').
consumo('salvia', 'Infusión').
consumo('sen', 'Extracto').

uso_terapeutico('ruda', 'te').
uso_terapeutico('ruibarbo', 'tintura').
uso_terapeutico('sen', 'te').
uso_terapeutico('sen', 'maceracion').
uso_terapeutico('salvia', 'te').

efecto_secundario('ruibarbo', 'intoxicacion').
efecto_secundario('ruibarbo', 'vomito').
efecto_secundario('ruda', 'abortiva').
efecto_secundario('ruda', 'desinfecta').
efecto_secundario('sen', 'purga').
efecto_secundario('sen', 'dolor_estomacal').
efecto_secundario('salvia', 'producir_alucinaciones').
efecto_secundario('salvia', 'abortiva').

precaucion('ruda', 'no_usar_durante_el_embarazo').
precaucion('sen', 'no_ingerir_en_exceso').
precaucion('salvia', 'ingerir_dosis_recomendada').
precaucion('ruibarbo', 'ingerir_dosis_recomendada').

origen('ruda', 'Europa').
origen('ruibarbo', 'Asia').
origen('salvia', 'Mediterráneo').
origen('sen', 'África').

trata_enfermedad('ruda', 'problemas_hepaticos').
trata_enfermedad('ruibarbo', 'problemas_digestivos').
trata_enfermedad('salvia', 'dolores_menstruales').
trata_enfermedad('sen', 'constipacion').

concepto('analgesica', 'Alivia el dolor').
concepto('antidiarreica', 'Combate la diarrea').
concepto('antiespasmodica', 'Reduce los espasmos musculares').
concepto('antiflamatoria', 'Reduce la inflamación').
concepto('antipiretica', 'Reduce la fiebre').
concepto('digestiva', 'Mejora la digestión').
concepto('diuretica', 'Aumenta la producción de orina').
concepto('purgante', 'Laxante fuerte').
concepto('sedante', 'Calma y relaja').

descripcion_enfermedad('gastritis', 'Inflamación de la mucosa gástrica').
descripcion_enfermedad('diarrea', 'Evacuaciones intestinales frecuentes y líquidas').
descripcion_enfermedad('insomnio', 'Dificultad para dormir').
descripcion_enfermedad('constipacion', 'Dificultad para evacuar el intestino').
descripcion_enfermedad('calculos_biliares', 'Piedras en la vesícula biliar').
descripcion_enfermedad('hepatitis', 'Inflamación del hígado').
descripcion_enfermedad('inflamaciones', 'Reacción del cuerpo a una lesión o infección').
descripcion_enfermedad('fatiga_cronica', 'Sensación persistente de cansancio').
descripcion_enfermedad('debilidad', 'Falta de fuerza física').
descripcion_enfermedad('gastritis', 'Inflamación de la mucosa del estómago').

% Funcion para arrancar la interfaz
start :-
    new(D, dialog('YERBERITO ILUSTRADO')),
    send(D, size, size(800, 800)),
    send(D, colour, colour(blue)),
    send(D, append, new(Menu, menu_bar)),
    send(Menu, append, new(PlantasMenu, popup(planta))),
    send(Menu, append, new(PalabrasClave, popup(diccionario))),
    send(Menu, append, new(EnfermedadMenu, popup(enfermedad))),
    send(Menu, append, new(Referencias, popup(referencias))),
    send(Menu, append, new(DatosImportantes, popup(datos_importantes))),

    % Opciones para buscar plantas
    send(PlantasMenu, append, menu_item(ruda, message(@prolog, mostrar_planta_info, ruda))),
    send(PlantasMenu, append, menu_item(ruibarbo, message(@prolog, mostrar_planta_info, ruibarbo))),
    send(PlantasMenu, append, menu_item(salvia, message(@prolog, mostrar_planta_info, salvia))),
    send(PlantasMenu, append, menu_item(sen, message(@prolog, mostrar_planta_info, sen))),

    % Opciones para buscar palabras clave
    send(PalabrasClave, append, menu_item(afrodisiaca, message(@prolog, mostrar_diccionario, afrodisiaca))),
    send(PalabrasClave, append, menu_item(analgesica, message(@prolog, mostrar_diccionario, analgesica))),
    send(PalabrasClave, append, menu_item(antidiarreica, message(@prolog, mostrar_diccionario, antidiarreica))),
    send(PalabrasClave, append, menu_item(antiespasmodica, message(@prolog, mostrar_diccionario, antiespasmodica))),
    send(PalabrasClave, append, menu_item(antiflamatoria, message(@prolog, mostrar_diccionario, antiflamatoria))),
    send(PalabrasClave, append, menu_item(antipiretica, message(@prolog, mostrar_diccionario, antipiretica))),
    send(PalabrasClave, append, menu_item(digestiva, message(@prolog, mostrar_diccionario, digestiva))),
    send(PalabrasClave, append, menu_item(diuretica, message(@prolog, mostrar_diccionario, diuretica))),
    send(PalabrasClave, append, menu_item(purgante, message(@prolog, mostrar_diccionario, purgante))),
    send(PalabrasClave, append, menu_item(sedante, message(@prolog, mostrar_diccionario, sedante))),

    % Opciones para buscar enfermedades
    send(EnfermedadMenu, append, menu_item(gastritis, message(@prolog, mostrar_enfermedad, gastritis))),
    send(EnfermedadMenu, append, menu_item(diarrea, message(@prolog, mostrar_enfermedad, diarrea))),
    send(EnfermedadMenu, append, menu_item(insomnio, message(@prolog, mostrar_enfermedad, insomnio))),
    send(EnfermedadMenu, append, menu_item(constipacion, message(@prolog, mostrar_enfermedad, constipacion))),
    send(EnfermedadMenu, append, menu_item(calculos_biliares, message(@prolog, mostrar_enfermedad, calculos_biliares))),
    send(EnfermedadMenu, append, menu_item(hepatitis, message(@prolog, mostrar_enfermedad, hepatitis))),
    send(EnfermedadMenu, append, menu_item(inflamaciones, message(@prolog, mostrar_enfermedad, inflamaciones))),
    send(EnfermedadMenu, append, menu_item(fatiga_cronica, message(@prolog, mostrar_enfermedad, fatiga_cronica))),
    send(EnfermedadMenu, append, menu_item(debilidad, message(@prolog, mostrar_enfermedad, debilidad))),

    % Opción para créditos y datos importantes
    send(Referencias, append, menu_item(referencias, message(@prolog, mostrar_referencias))),
    send(DatosImportantes, append, menu_item(datos_importantes, message(@prolog, mostrar_datos_importantes))),

    % Mostrar imagen de fondo y abrir la interfaz
    mostrar('C:/Yerberus/imgs/YerberitoPortada.jpg', D, Menu),
    send(D, open_centered, point(0, 0)).

% Iniciar la aplicación
:- start.
