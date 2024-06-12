/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.ListPath;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QHabilitationIHMRoute is a Querydsl query type for HabilitationIHMRoute.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QHabilitationIHMRoute extends EntityPathBase<HabilitationIHMRoute> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -250856016L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant habilitationIHMRoute. */
	public static final QHabilitationIHMRoute habilitationIHMRoute =
			new QHabilitationIHMRoute("habilitationIHMRoute");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The client. */
	public final StringPath client = createString("client");

	/** The code IHM route. */
	public final StringPath codeIHMRoute = createString("codeIHMRoute");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The forms. */
	public final SetPath<AcmIhmForm, QAcmIhmForm> forms = this.<AcmIhmForm, QAcmIhmForm>createSet(
			"forms", AcmIhmForm.class, QAcmIhmForm.class, PathInits.DIRECT2);

	/** The habilitation. */
	public final ListPath<Habilitation, QHabilitation> habilitation =
			this.<Habilitation, QHabilitation>createList("habilitation", Habilitation.class,
					QHabilitation.class, PathInits.DIRECT2);

	/** The habilitation IHM buttons. */
	public final SetPath<HabilitationIHMButton, QHabilitationIHMButton> habilitationIHMButtons =
			this.<HabilitationIHMButton, QHabilitationIHMButton>createSet("habilitationIHMButtons",
					HabilitationIHMButton.class, QHabilitationIHMButton.class, PathInits.DIRECT2);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The ihm route. */
	public final StringPath ihmRoute = createString("ihmRoute");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The racine id. */
	public final QAcmModule racineId;

	/** The settings workflow. */
	public final BooleanPath settingsWorkflow = createBoolean("settingsWorkflow");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q habilitation IHM route.
	 *
	 * @param variable the variable
	 */
	public QHabilitationIHMRoute(String variable) {

		this(HabilitationIHMRoute.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q habilitation IHM route.
	 *
	 * @param path the path
	 */
	public QHabilitationIHMRoute(Path<? extends HabilitationIHMRoute> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q habilitation IHM route.
	 *
	 * @param metadata the metadata
	 */
	public QHabilitationIHMRoute(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q habilitation IHM route.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QHabilitationIHMRoute(PathMetadata metadata, PathInits inits) {

		this(HabilitationIHMRoute.class, metadata, inits);
	}

	/**
	 * Instantiates a new q habilitation IHM route.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QHabilitationIHMRoute(Class<? extends HabilitationIHMRoute> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.racineId =
				inits.isInitialized("racineId") ? new QAcmModule(forProperty("racineId")) : null;
	}

}
