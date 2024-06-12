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
 * QUser is a Querydsl query type for User.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUser extends EntityPathBase<User> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1924732898L;

	/** The Constant user. */
	public static final QUser user = new QUser("user");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The access branches. */
	public final StringPath accessBranches = createString("accessBranches");

	/** The account portfolio id. */
	public final NumberPath<Long> accountPortfolioId =
			createNumber("accountPortfolioId", Long.class);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

	/** The branch ID. */
	public final NumberPath<Integer> branchID = createNumber("branchID", Integer.class);

	/** The branch name. */
	public final StringPath branchName = createString("branchName");

	/** The category. */
	public final StringPath category = createString("category");

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The default lang. */
	public final StringPath defaultLang = createString("defaultLang");

	/** The email. */
	public final StringPath email = createString("email");

	/** The employee id. */
	public final StringPath employeeId = createString("employeeId");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The groupes. */
	public final SetPath<Groupe, QGroupe> groupes = this.<Groupe, QGroupe>createSet("groupes",
			Groupe.class, QGroupe.class, PathInits.DIRECT2);

	/** The hiring date. */
	public final DateTimePath<java.util.Date> hiringDate =
			createDateTime("hiringDate", java.util.Date.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The nom. */
	public final StringPath nom = createString("nom");

	/** The old responsable name. */
	public final StringPath oldResponsableName = createString("oldResponsableName");

	/** The old responsible id. */
	public final StringPath oldResponsibleId = createString("oldResponsibleId");

	/** The password. */
	public final StringPath password = createString("password");

	/** The portfolio name. */
	public final StringPath portfolioName = createString("portfolioName");

	/** The prenom. */
	public final StringPath prenom = createString("prenom");

	/** The resigning date. */
	public final DateTimePath<java.util.Date> resigningDate =
			createDateTime("resigningDate", java.util.Date.class);

	/** The responsable id. */
	public final StringPath responsableId = createString("responsableId");

	/** The temporary pwd. */
	public final BooleanPath temporaryPwd = createBoolean("temporaryPwd");

	/** The temporary responsable. */
	public final BooleanPath temporaryResponsable = createBoolean("temporaryResponsable");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user extern id. */
	public final NumberPath<Long> userExternId = createNumber("userExternId", Long.class);

	/** The username. */
	public final StringPath username = createString("username");

	/** The user profil id. */
	public final NumberPath<Long> userProfilId = createNumber("userProfilId", Long.class);

	/** The failed attempts. */
	public final NumberPath<Integer> failedAttempts = createNumber("failedAttempts", Integer.class);

	/** The pwd expiry date. */
	public final DateTimePath<java.util.Date> pwdExpiryDate =
			createDateTime("pwdExpiryDate", java.util.Date.class);

	/**
	 * Instantiates a new q user.
	 *
	 * @param variable the variable
	 */
	public QUser(String variable) {

		super(User.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q user.
	 *
	 * @param path the path
	 */
	public QUser(Path<? extends User> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q user.
	 *
	 * @param metadata the metadata
	 */
	public QUser(PathMetadata metadata) {

		super(User.class, metadata);
	}

}
