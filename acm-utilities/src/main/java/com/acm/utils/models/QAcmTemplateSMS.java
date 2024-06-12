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
 * QAcmTemplateSMS is a Querydsl query type for AcmTemplateSMS.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmTemplateSMS extends EntityPathBase<AcmTemplateSMS> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1283694667L;

	/** The Constant acmTemplateSMS. */
	public static final QAcmTemplateSMS acmTemplateSMS = new QAcmTemplateSMS("acmTemplateSMS");

	/** The category. */
	public final StringPath category = createString("category");

	/** The code SMS event. */
	public final StringPath codeSMSEvent = createString("codeSMSEvent");

	/** The Date envoi. */
	public final StringPath DateEnvoi = createString("DateEnvoi");

	/** The id acm template SMS. */
	public final NumberPath<Long> idAcmTemplateSMS = createNumber("idAcmTemplateSMS", Long.class);

	/** The message body. */
	public final StringPath messageBody = createString("messageBody");

	/**
	 * Instantiates a new q acm template SMS.
	 *
	 * @param variable the variable
	 */
	public QAcmTemplateSMS(String variable) {

		super(AcmTemplateSMS.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm template SMS.
	 *
	 * @param path the path
	 */
	public QAcmTemplateSMS(Path<? extends AcmTemplateSMS> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm template SMS.
	 *
	 * @param metadata the metadata
	 */
	public QAcmTemplateSMS(PathMetadata metadata) {

		super(AcmTemplateSMS.class, metadata);
	}

}
