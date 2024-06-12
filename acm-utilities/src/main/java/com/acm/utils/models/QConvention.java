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

/**
 * QConvention is a Querydsl query type for Convention.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QConvention extends EntityPathBase<Convention> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -713958840L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant convention. */
	public static final QConvention convention = new QConvention("convention");

	/** The apply rate. */
	public final BooleanPath applyRate = createBoolean("applyRate");

	/** The ca. */
	public final NumberPath<Long> ca = createNumber("ca", Long.class);

	/** The discount rate. */
	public final NumberPath<Long> discountRate = createNumber("discountRate", Long.class);

	/** The end date convention. */
	public final DateTimePath<java.util.Date> endDateConvention =
			createDateTime("endDateConvention", java.util.Date.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The objective file number. */
	public final NumberPath<Long> objectiveFileNumber =
			createNumber("objectiveFileNumber", Long.class);

	/** The objective turnover volume. */
	public final NumberPath<Long> objectiveTurnoverVolume =
			createNumber("objectiveTurnoverVolume", Long.class);

	/** The rebate. */
	public final NumberPath<Long> rebate = createNumber("rebate", Long.class);

	/** The start date convention. */
	public final DateTimePath<java.util.Date> startDateConvention =
			createDateTime("startDateConvention", java.util.Date.class);

	/** The supplier. */
	public final QSupplier supplier;

	/**
	 * Instantiates a new q convention.
	 *
	 * @param variable the variable
	 */
	public QConvention(String variable) {

		this(Convention.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q convention.
	 *
	 * @param path the path
	 */
	public QConvention(Path<? extends Convention> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q convention.
	 *
	 * @param metadata the metadata
	 */
	public QConvention(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q convention.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QConvention(PathMetadata metadata, PathInits inits) {

		this(Convention.class, metadata, inits);
	}

	/**
	 * Instantiates a new q convention.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QConvention(Class<? extends Convention> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.supplier =
				inits.isInitialized("supplier") ? new QSupplier(forProperty("supplier")) : null;
	}

}
