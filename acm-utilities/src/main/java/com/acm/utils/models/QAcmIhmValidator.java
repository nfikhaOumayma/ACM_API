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
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QAcmIhmValidator is a Querydsl query type for AcmIhmValidator.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmIhmValidator extends EntityPathBase<AcmIhmValidator> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1534348152L;

	/** The Constant acmIhmValidator. */
	public static final QAcmIhmValidator acmIhmValidator = new QAcmIhmValidator("acmIhmValidator");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code validator. */
	public final StringPath codeValidator = createString("codeValidator");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The fields. */
	public final SetPath<AcmIhmField, QAcmIhmField> fields =
			this.<AcmIhmField, QAcmIhmField>createSet("fields", AcmIhmField.class,
					QAcmIhmField.class, PathInits.DIRECT2);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The parameter. */
	public final StringPath parameter = createString("parameter");

	/** The type validator. */
	public final StringPath typeValidator = createString("typeValidator");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm ihm validator.
	 *
	 * @param variable the variable
	 */
	public QAcmIhmValidator(String variable) {

		super(AcmIhmValidator.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm ihm validator.
	 *
	 * @param path the path
	 */
	public QAcmIhmValidator(Path<? extends AcmIhmValidator> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm ihm validator.
	 *
	 * @param metadata the metadata
	 */
	public QAcmIhmValidator(PathMetadata metadata) {

		super(AcmIhmValidator.class, metadata);
	}

}
