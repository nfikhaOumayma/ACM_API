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
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QHabilitationIHMButton is a Querydsl query type for HabilitationIHMButton.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QHabilitationIHMButton extends EntityPathBase<HabilitationIHMButton> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 360843435L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant habilitationIHMButton. */
	public static final QHabilitationIHMButton habilitationIHMButton =
			new QHabilitationIHMButton("habilitationIHMButton");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The client. */
	public final StringPath client = createString("client");

	/** The code ihm button. */
	public final StringPath codeIhmButton = createString("codeIhmButton");

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

	/** The habilitation IHM route. */
	public final QHabilitationIHMRoute habilitationIHMRoute;

	/** The id habilitation ihm button. */
	public final NumberPath<Long> idHabilitationIhmButton =
			createNumber("idHabilitationIhmButton", Long.class);

	/** The ihm button. */
	public final StringPath ihmButton = createString("ihmButton");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q habilitation IHM button.
	 *
	 * @param variable the variable
	 */
	public QHabilitationIHMButton(String variable) {

		this(HabilitationIHMButton.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q habilitation IHM button.
	 *
	 * @param path the path
	 */
	public QHabilitationIHMButton(Path<? extends HabilitationIHMButton> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q habilitation IHM button.
	 *
	 * @param metadata the metadata
	 */
	public QHabilitationIHMButton(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q habilitation IHM button.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QHabilitationIHMButton(PathMetadata metadata, PathInits inits) {

		this(HabilitationIHMButton.class, metadata, inits);
	}

	/**
	 * Instantiates a new q habilitation IHM button.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QHabilitationIHMButton(Class<? extends HabilitationIHMButton> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.habilitationIHMRoute = inits.isInitialized("habilitationIHMRoute")
				? new QHabilitationIHMRoute(forProperty("habilitationIHMRoute"))
				: null;
	}

}
