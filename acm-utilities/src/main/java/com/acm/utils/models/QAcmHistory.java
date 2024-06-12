/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * {@link QAcmHistory} class.
 *
 * @author hchaouachi
 * @since 0.1.0
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmHistory extends EntityPathBase<AcmHistory> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 394518624L;

	/** The Constant acmHistory. */
	public static final QAcmHistory acmHistory = new QAcmHistory("acmHistory");

	/** The action by. */
	public final StringPath actionBy = createString("actionBy");

	/** The actions. */
	public final StringPath actions = createString("actions");

	/** The date action. */
	public final DateTimePath<java.util.Date> dateAction =
			createDateTime("dateAction", java.util.Date.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id object. */
	public final NumberPath<Long> idObject = createNumber("idObject", Long.class);

	/** The value object. */
	public final StringPath valueObject = createString("valueObject");

	/**
	 * Instantiates a new q acm history.
	 *
	 * @param variable the variable
	 */
	public QAcmHistory(String variable) {

		super(AcmHistory.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm history.
	 *
	 * @param path the path
	 */
	public QAcmHistory(Path<? extends AcmHistory> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm history.
	 *
	 * @param metadata the metadata
	 */
	public QAcmHistory(PathMetadata metadata) {

		super(AcmHistory.class, metadata);
	}

}
