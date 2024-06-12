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
 * QAcmEnvironnement is a Querydsl query type for AcmEnvironnement.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmEnvironnement extends EntityPathBase<AcmEnvironnement> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 245638614L;

	/** The Constant acmEnvironnement. */
	public static final QAcmEnvironnement acmEnvironnement =
			new QAcmEnvironnement("acmEnvironnement");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category. */
	public final StringPath category = createString("category");

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

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The key. */
	public final StringPath key = createString("key");

	/** The type. */
	public final StringPath type = createString("type");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value. */
	public final StringPath value = createString("value");

	/**
	 * Instantiates a new q acm environnement.
	 *
	 * @param variable the variable
	 */
	public QAcmEnvironnement(String variable) {

		super(AcmEnvironnement.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm environnement.
	 *
	 * @param path the path
	 */
	public QAcmEnvironnement(Path<? extends AcmEnvironnement> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm environnement.
	 *
	 * @param metadata the metadata
	 */
	public QAcmEnvironnement(PathMetadata metadata) {

		super(AcmEnvironnement.class, metadata);
	}

}
