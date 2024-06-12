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
 * QAssetLoan is a Querydsl query type for AssetLoan.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAssetLoan extends EntityPathBase<AssetLoan> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1503327817L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant assetLoan. */
	public static final QAssetLoan assetLoan = new QAssetLoan("assetLoan");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The asset. */
	public final QAsset asset;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan. */
	public final QLoan loan;

	/** The prix unitaire. */
	public final NumberPath<java.math.BigDecimal> prixUnitaire =
			createNumber("prixUnitaire", java.math.BigDecimal.class);

	/** The quantite article. */
	public final NumberPath<Integer> quantiteArticle =
			createNumber("quantiteArticle", Integer.class);

	/** The remise article. */
	public final NumberPath<java.math.BigDecimal> remiseArticle =
			createNumber("remiseArticle", java.math.BigDecimal.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q asset loan.
	 *
	 * @param variable the variable
	 */
	public QAssetLoan(String variable) {

		this(AssetLoan.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q asset loan.
	 *
	 * @param path the path
	 */
	public QAssetLoan(Path<? extends AssetLoan> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q asset loan.
	 *
	 * @param metadata the metadata
	 */
	public QAssetLoan(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q asset loan.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAssetLoan(PathMetadata metadata, PathInits inits) {

		this(AssetLoan.class, metadata, inits);
	}

	/**
	 * Instantiates a new q asset loan.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAssetLoan(Class<? extends AssetLoan> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.asset =
				inits.isInitialized("asset") ? new QAsset(forProperty("asset"), inits.get("asset"))
						: null;
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}
