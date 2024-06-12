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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSimahClassType is a Querydsl query type for SimahClassType.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSimahClassType extends EntityPathBase<SimahClassType> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 396747403L;

	/** The Constant simahClassType. */
	public static final QSimahClassType simahClassType = new QSimahClassType("simahClassType");

	/** The class description. */
	public final StringPath classDescription = createString("classDescription");

	/** The class label. */
	public final StringPath classLabel = createString("classLabel");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/**
	 * Instantiates a new q simah class type.
	 *
	 * @param variable the variable
	 */
	public QSimahClassType(String variable) {

		super(SimahClassType.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q simah class type.
	 *
	 * @param path the path
	 */
	public QSimahClassType(Path<? extends SimahClassType> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q simah class type.
	 *
	 * @param metadata the metadata
	 */
	public QSimahClassType(PathMetadata metadata) {

		super(SimahClassType.class, metadata);
	}

}
