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
 * QUsersNotifications is a Querydsl query type for UsersNotifications.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUsersNotifications extends EntityPathBase<UsersNotifications> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1219495017L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant usersNotifications. */
	public static final QUsersNotifications usersNotifications =
			new QUsersNotifications("usersNotifications");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id users notification. */
	public final NumberPath<Long> idUsersNotification =
			createNumber("idUsersNotification", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The setting notification. */
	public final QSettingNotifications settingNotification;

	/** The statut. */
	public final BooleanPath statut = createBoolean("statut");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user. */
	public final QUser user;

	/**
	 * Instantiates a new q users notifications.
	 *
	 * @param variable the variable
	 */
	public QUsersNotifications(String variable) {

		this(UsersNotifications.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q users notifications.
	 *
	 * @param path the path
	 */
	public QUsersNotifications(Path<? extends UsersNotifications> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q users notifications.
	 *
	 * @param metadata the metadata
	 */
	public QUsersNotifications(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q users notifications.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QUsersNotifications(PathMetadata metadata, PathInits inits) {

		this(UsersNotifications.class, metadata, inits);
	}

	/**
	 * Instantiates a new q users notifications.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QUsersNotifications(Class<? extends UsersNotifications> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.settingNotification = inits.isInitialized("settingNotification")
				? new QSettingNotifications(forProperty("settingNotification"))
				: null;
		this.user = inits.isInitialized("user") ? new QUser(forProperty("user")) : null;
	}

}
