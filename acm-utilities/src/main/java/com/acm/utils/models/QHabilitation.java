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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QHabilitation is a Querydsl query type for Habilitation.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QHabilitation extends EntityPathBase<Habilitation> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 363496149L;

	/** The Constant habilitation. */
	public static final QHabilitation habilitation = new QHabilitation("habilitation");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm habilitation. */
	public final StringPath acmHabilitation = createString("acmHabilitation");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The acm web route. */
	public final StringPath acmWebRoute = createString("acmWebRoute");

	/** The actions. */
	public final StringPath actions = createString("actions");

	/** The client. */
	public final StringPath client = createString("client");

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id groupe. */
	public final NumberPath<Long> idGroupe = createNumber("idGroupe", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value. */
	public final StringPath value = createString("value");

	/**
	 * Instantiates a new q habilitation.
	 *
	 * @param variable the variable
	 */
	public QHabilitation(String variable) {

		super(Habilitation.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q habilitation.
	 *
	 * @param path the path
	 */
	public QHabilitation(Path<? extends Habilitation> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q habilitation.
	 *
	 * @param metadata the metadata
	 */
	public QHabilitation(PathMetadata metadata) {

		super(Habilitation.class, metadata);
	}

}
