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

import com.querydsl.core.types.dsl.PathInits;

/**
 * QAcmIhmFieldGroupe is a Querydsl query type for AcmIhmFieldGroupe.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmIhmFieldGroupe extends EntityPathBase<AcmIhmFieldGroupe> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1596321274L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmIhmFieldGroupe. */
	public static final QAcmIhmFieldGroupe acmIhmFieldGroupe =
			new QAcmIhmFieldGroupe("acmIhmFieldGroupe");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm ihm field. */
	public final QAcmIhmField acmIhmField;

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The group. */
	public final QGroupe group;

	/** The habilitation. */
	public final StringPath habilitation = createString("habilitation");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm ihm field groupe.
	 *
	 * @param variable the variable
	 */
	public QAcmIhmFieldGroupe(String variable) {

		this(AcmIhmFieldGroupe.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm ihm field groupe.
	 *
	 * @param path the path
	 */
	public QAcmIhmFieldGroupe(Path<? extends AcmIhmFieldGroupe> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm ihm field groupe.
	 *
	 * @param metadata the metadata
	 */
	public QAcmIhmFieldGroupe(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm ihm field groupe.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmFieldGroupe(PathMetadata metadata, PathInits inits) {

		this(AcmIhmFieldGroupe.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm ihm field groupe.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmFieldGroupe(Class<? extends AcmIhmFieldGroupe> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.acmIhmField = inits.isInitialized("acmIhmField")
				? new QAcmIhmField(forProperty("acmIhmField"), inits.get("acmIhmField"))
				: null;
		this.group = inits.isInitialized("group") ? new QGroupe(forProperty("group")) : null;
	}

}
