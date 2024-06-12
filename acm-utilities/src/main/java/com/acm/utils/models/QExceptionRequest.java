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
 * QExceptionRequest is a Querydsl query type for ExceptionRequest.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QExceptionRequest extends EntityPathBase<ExceptionRequest> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -625978153L;

	/** The Constant exceptionRequest. */
	public static final QExceptionRequest exceptionRequest =
			new QExceptionRequest("exceptionRequest");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The allowed amount. */
	public final NumberPath<java.math.BigDecimal> allowedAmount =
			createNumber("allowedAmount", java.math.BigDecimal.class);

	/** The branch id. */
	public final NumberPath<Integer> branchId = createNumber("branchId", Integer.class);

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The group owner code. */
	public final StringPath groupOwnerCode = createString("groupOwnerCode");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The Maker name. */
	public final StringPath MakerName = createString("MakerName");

	/** The maker username. */
	public final StringPath makerUsername = createString("makerUsername");

	/** The note. */
	public final StringPath note = createString("note");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The owner username. */
	public final StringPath ownerUsername = createString("ownerUsername");

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The product limit. */
	public final NumberPath<java.math.BigDecimal> productLimit =
			createNumber("productLimit", java.math.BigDecimal.class);

	/** The product name. */
	public final StringPath productName = createString("productName");

	/** The reject note. */
	public final StringPath rejectNote = createString("rejectNote");

	/** The requested amount. */
	public final NumberPath<java.math.BigDecimal> requestedAmount =
			createNumber("requestedAmount", java.math.BigDecimal.class);

	/** The statut. */
	public final NumberPath<Integer> statut = createNumber("statut", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q exception request.
	 *
	 * @param variable the variable
	 */
	public QExceptionRequest(String variable) {

		super(ExceptionRequest.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q exception request.
	 *
	 * @param path the path
	 */
	public QExceptionRequest(Path<? extends ExceptionRequest> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q exception request.
	 *
	 * @param metadata the metadata
	 */
	public QExceptionRequest(PathMetadata metadata) {

		super(ExceptionRequest.class, metadata);
	}

}
