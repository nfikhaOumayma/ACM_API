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
 * QAcmDamagedData is a Querydsl query type for AcmDamagedData.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmDamagedData extends EntityPathBase<AcmDamagedData> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 340068843L;

	/** The Constant acmDamagedData. */
	public static final QAcmDamagedData acmDamagedData = new QAcmDamagedData("acmDamagedData");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The damaged data. */
	public final StringPath damagedData = createString("damagedData");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The exception details. */
	public final StringPath exceptionDetails = createString("exceptionDetails");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id account extern. */
	public final NumberPath<Long> idAccountExtern = createNumber("idAccountExtern", Long.class);

	/** The id customer externe. */
	public final NumberPath<Long> idCustomerExterne = createNumber("idCustomerExterne", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm damaged data.
	 *
	 * @param variable the variable
	 */
	public QAcmDamagedData(String variable) {

		super(AcmDamagedData.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm damaged data.
	 *
	 * @param path the path
	 */
	public QAcmDamagedData(Path<? extends AcmDamagedData> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm damaged data.
	 *
	 * @param metadata the metadata
	 */
	public QAcmDamagedData(PathMetadata metadata) {

		super(AcmDamagedData.class, metadata);
	}

}