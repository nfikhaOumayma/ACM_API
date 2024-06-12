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
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSettingNotifications is a Querydsl query type for SettingNotifications.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingNotifications extends EntityPathBase<SettingNotifications> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -306604401L;

	/** The Constant settingNotifications. */
	public static final QSettingNotifications settingNotifications =
			new QSettingNotifications("settingNotifications");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category. */
	public final StringPath category = createString("category");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id setting notification. */
	public final NumberPath<Long> idSettingNotification =
			createNumber("idSettingNotification", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The type notif. */
	public final StringPath typeNotif = createString("typeNotif");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The users notifications. */
	public final SetPath<UsersNotifications, QUsersNotifications> usersNotifications =
			this.<UsersNotifications, QUsersNotifications>createSet("usersNotifications",
					UsersNotifications.class, QUsersNotifications.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q setting notifications.
	 *
	 * @param variable the variable
	 */
	public QSettingNotifications(String variable) {

		super(SettingNotifications.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting notifications.
	 *
	 * @param path the path
	 */
	public QSettingNotifications(Path<? extends SettingNotifications> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting notifications.
	 *
	 * @param metadata the metadata
	 */
	public QSettingNotifications(PathMetadata metadata) {

		super(SettingNotifications.class, metadata);
	}

}
