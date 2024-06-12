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
 * QNotifications is a Querydsl query type for Notifications.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QNotifications extends EntityPathBase<Notifications> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 496964273L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant notifications. */
	public static final QNotifications notifications = new QNotifications("notifications");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action. */
	public final StringPath action = createString("action");

	/** The calendar event. */
	public final QCalendarEvent calendarEvent;

	/** The category. */
	public final StringPath category = createString("category");

	/** The creaction date. */
	public final DateTimePath<java.util.Date> creactionDate =
			createDateTime("creactionDate", java.util.Date.class);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id acm collection. */
	public final NumberPath<Long> idAcmCollection = createNumber("idAcmCollection", Long.class);

	/** The id notification. */
	public final NumberPath<Long> idNotification = createNumber("idNotification", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan. */
	public final QLoan loan;

	/** The redirect. */
	public final BooleanPath redirect = createBoolean("redirect");

	/** The status notif. */
	public final StringPath statusNotif = createString("statusNotif");

	/** The target date. */
	public final DateTimePath<java.util.Date> targetDate =
			createDateTime("targetDate", java.util.Date.class);

	/** The title. */
	public final StringPath title = createString("title");

	/** The type motif. */
	public final StringPath typeMotif = createString("typeMotif");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The username. */
	public final StringPath username = createString("username");

	/**
	 * Instantiates a new q notifications.
	 *
	 * @param variable the variable
	 */
	public QNotifications(String variable) {

		this(Notifications.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q notifications.
	 *
	 * @param path the path
	 */
	public QNotifications(Path<? extends Notifications> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q notifications.
	 *
	 * @param metadata the metadata
	 */
	public QNotifications(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q notifications.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QNotifications(PathMetadata metadata, PathInits inits) {

		this(Notifications.class, metadata, inits);
	}

	/**
	 * Instantiates a new q notifications.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QNotifications(Class<? extends Notifications> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.calendarEvent = inits.isInitialized("calendarEvent")
				? new QCalendarEvent(forProperty("calendarEvent"))
				: null;
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}
