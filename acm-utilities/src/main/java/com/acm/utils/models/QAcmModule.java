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
import com.querydsl.core.types.dsl.ListPath;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QAcmModule is a Querydsl query type for AcmModule.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmModule extends EntityPathBase<AcmModule> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1962082592L;

	/** The Constant acmModule. */
	public static final QAcmModule acmModule = new QAcmModule("acmModule");

	/** The habilitation IHM route. */
	public final ListPath<HabilitationIHMRoute, QHabilitationIHMRoute> habilitationIHMRoute =
			this.<HabilitationIHMRoute, QHabilitationIHMRoute>createList("habilitationIHMRoute",
					HabilitationIHMRoute.class, QHabilitationIHMRoute.class, PathInits.DIRECT2);

	/** The id module. */
	public final NumberPath<Long> idModule = createNumber("idModule", Long.class);

	/** The module. */
	public final StringPath module = createString("module");

	/**
	 * Instantiates a new q acm module.
	 *
	 * @param variable the variable
	 */
	public QAcmModule(String variable) {

		super(AcmModule.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm module.
	 *
	 * @param path the path
	 */
	public QAcmModule(Path<? extends AcmModule> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm module.
	 *
	 * @param metadata the metadata
	 */
	public QAcmModule(PathMetadata metadata) {

		super(AcmModule.class, metadata);
	}

}
