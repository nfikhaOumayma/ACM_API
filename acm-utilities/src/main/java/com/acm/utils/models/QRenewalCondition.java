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
 * QRenewalCondition is a Querydsl query type for RenewalCondition.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QRenewalCondition extends EntityPathBase<RenewalCondition> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 289181210L;

	/** The Constant renewalCondition. */
	public static final QRenewalCondition renewalCondition =
			new QRenewalCondition("renewalCondition");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The max amount. */
	public final NumberPath<Long> maxAmount = createNumber("maxAmount", Long.class);

	/** The min amount. */
	public final NumberPath<Long> minAmount = createNumber("minAmount", Long.class);

	/** The ordre. */
	public final NumberPath<Long> ordre = createNumber("ordre", Long.class);
	/** The pourcentage. */
	public final NumberPath<Integer> pourcentage = createNumber("pourcentage", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The year. */
	public final NumberPath<Integer> year = createNumber("year", Integer.class);

	/**
	 * Instantiates a new q renewal condition.
	 *
	 * @param variable the variable
	 */
	public QRenewalCondition(String variable) {

		super(RenewalCondition.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q renewal condition.
	 *
	 * @param path the path
	 */
	public QRenewalCondition(Path<? extends RenewalCondition> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q renewal condition.
	 *
	 * @param metadata the metadata
	 */
	public QRenewalCondition(PathMetadata metadata) {

		super(RenewalCondition.class, metadata);
	}

}
