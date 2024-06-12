/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;

/**
 * QSimahClassDetails is a Querydsl query type for SimahClassDetails.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSimahClassDetails extends EntityPathBase<SimahClassDetails> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2132176559L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant simahClassDetails. */
	public static final QSimahClassDetails simahClassDetails =
			new QSimahClassDetails("simahClassDetails");

	/** The id. */
	public final NumberPath<Integer> id = createNumber("id", Integer.class);

	/** The rate. */
	public final NumberPath<Integer> rate = createNumber("rate", Integer.class);

	/** The salary from. */
	public final NumberPath<java.math.BigDecimal> salaryFrom =
			createNumber("salaryFrom", java.math.BigDecimal.class);

	/** The salary to. */
	public final NumberPath<java.math.BigDecimal> salaryTo =
			createNumber("salaryTo", java.math.BigDecimal.class);

	/** The simah class type. */
	public final QSimahClassType simahClassType;

	/**
	 * Instantiates a new q simah class details.
	 *
	 * @param variable the variable
	 */
	public QSimahClassDetails(String variable) {

		this(SimahClassDetails.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q simah class details.
	 *
	 * @param path the path
	 */
	public QSimahClassDetails(Path<? extends SimahClassDetails> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q simah class details.
	 *
	 * @param metadata the metadata
	 */
	public QSimahClassDetails(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q simah class details.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSimahClassDetails(PathMetadata metadata, PathInits inits) {

		this(SimahClassDetails.class, metadata, inits);
	}

	/**
	 * Instantiates a new q simah class details.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSimahClassDetails(Class<? extends SimahClassDetails> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.simahClassType = inits.isInitialized("simahClassType")
				? new QSimahClassType(forProperty("simahClassType"))
				: null;
	}

}
