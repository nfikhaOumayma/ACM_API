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
 * QAcmDamagedCustomer is a Querydsl query type for AcmDamagedCustomer.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmDamagedCustomer extends EntityPathBase<AcmDamagedCustomer> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1355902561L;

	/** The Constant acmDamagedCustomer. */
	public static final QAcmDamagedCustomer acmDamagedCustomer =
			new QAcmDamagedCustomer("acmDamagedCustomer");

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

	/** The id customer externe. */
	public final NumberPath<Long> idCustomerExterne = createNumber("idCustomerExterne", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm damaged customer.
	 *
	 * @param variable the variable
	 */
	public QAcmDamagedCustomer(String variable) {

		super(AcmDamagedCustomer.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm damaged customer.
	 *
	 * @param path the path
	 */
	public QAcmDamagedCustomer(Path<? extends AcmDamagedCustomer> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm damaged customer.
	 *
	 * @param metadata the metadata
	 */
	public QAcmDamagedCustomer(PathMetadata metadata) {

		super(AcmDamagedCustomer.class, metadata);
	}

}
