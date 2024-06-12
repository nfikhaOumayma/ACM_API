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
 * QProductDetails is a Querydsl query type for ProductDetails.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QProductDetails extends EntityPathBase<ProductDetails> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1138336342L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant productDetails. */
	public static final QProductDetails productDetails = new QProductDetails("productDetails");

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

	/** The deferred period types. */
	public final StringPath deferredPeriodTypes = createString("deferredPeriodTypes");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The maximum amount. */
	public final NumberPath<Long> maximumAmount = createNumber("maximumAmount", Long.class);

	/** The maximum term. */
	public final NumberPath<Integer> maximumTerm = createNumber("maximumTerm", Integer.class);

	/** The minimum amount. */
	public final NumberPath<Long> minimumAmount = createNumber("minimumAmount", Long.class);

	/** The minimum term. */
	public final NumberPath<Integer> minimumTerm = createNumber("minimumTerm", Integer.class);

	/** The product. */
	public final QProduct product;

	/** The term type. */
	public final StringPath termType = createString("termType");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q product details.
	 *
	 * @param variable the variable
	 */
	public QProductDetails(String variable) {

		this(ProductDetails.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q product details.
	 *
	 * @param path the path
	 */
	public QProductDetails(Path<? extends ProductDetails> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q product details.
	 *
	 * @param metadata the metadata
	 */
	public QProductDetails(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q product details.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QProductDetails(PathMetadata metadata, PathInits inits) {

		this(ProductDetails.class, metadata, inits);
	}

	/**
	 * Instantiates a new q product details.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QProductDetails(Class<? extends ProductDetails> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.product = inits.isInitialized("product") ? new QProduct(forProperty("product")) : null;
	}

}
