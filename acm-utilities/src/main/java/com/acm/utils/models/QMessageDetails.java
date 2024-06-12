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
 * QMessageDetails is a Querydsl query type for MessageDetails.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QMessageDetails extends EntityPathBase<MessageDetails> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -147456110L;

	/** The Constant messageDetails. */
	public static final QMessageDetails messageDetails = new QMessageDetails("messageDetails");

	/** The category. */
	public final StringPath category = createString("category");

	/** The date envoi. */
	public final DateTimePath<java.util.Date> dateEnvoi =
			createDateTime("dateEnvoi", java.util.Date.class);

	/** The id message details. */
	public final NumberPath<Long> idMessageDetails = createNumber("idMessageDetails", Long.class);

	/** The list id. */
	public final NumberPath<Integer> listId = createNumber("listId", Integer.class);

	/** The message body. */
	public final StringPath messageBody = createString("messageBody");

	/** The sending source. */
	public final StringPath sendingSource = createString("sendingSource");

	/** The sms campaign name. */
	public final StringPath smsCampaignName = createString("smsCampaignName");

	/** The to. */
	public final StringPath to = createString("to");

	/**
	 * Instantiates a new q message details.
	 *
	 * @param variable the variable
	 */
	public QMessageDetails(String variable) {

		super(MessageDetails.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q message details.
	 *
	 * @param path the path
	 */
	public QMessageDetails(Path<? extends MessageDetails> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q message details.
	 *
	 * @param metadata the metadata
	 */
	public QMessageDetails(PathMetadata metadata) {

		super(MessageDetails.class, metadata);
	}

}
