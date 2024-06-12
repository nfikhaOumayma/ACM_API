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
 * QCustomerContact is a Querydsl query type for CustomerContact.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCustomerContact extends EntityPathBase<CustomerContact> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 215381483L;

	/** The Constant customerContact. */
	public static final QCustomerContact customerContact = new QCustomerContact("customerContact");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The content. */
	public final StringPath content = createString("content");

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The email. */
	public final StringPath email = createString("email");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The from. */
	public final StringPath from = createString("from");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The link replay. */
	public final NumberPath<Long> linkReplay = createNumber("linkReplay", Long.class);

	/** The name. */
	public final StringPath name = createString("name");

	/** The phone. */
	public final StringPath phone = createString("phone");

	/** The priority. */
	public final NumberPath<Integer> priority = createNumber("priority", Integer.class);

	/** The read. */
	public final BooleanPath read = createBoolean("read");

	/** The sent customer. */
	public final BooleanPath sentCustomer = createBoolean("sentCustomer");

	/** The statut. */
	public final StringPath statut = createString("statut");

	/** The subject. */
	public final StringPath subject = createString("subject");

	/** The to. */
	public final StringPath to = createString("to");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user name. */
	public final StringPath userName = createString("userName");

	/**
	 * Instantiates a new q customer contact.
	 *
	 * @param variable the variable
	 */
	public QCustomerContact(String variable) {

		super(CustomerContact.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q customer contact.
	 *
	 * @param path the path
	 */
	public QCustomerContact(Path<? extends CustomerContact> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q customer contact.
	 *
	 * @param metadata the metadata
	 */
	public QCustomerContact(PathMetadata metadata) {

		super(CustomerContact.class, metadata);
	}

}
