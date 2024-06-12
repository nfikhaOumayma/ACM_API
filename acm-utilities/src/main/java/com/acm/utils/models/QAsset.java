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
 * QAsset is a Querydsl query type for Asset.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAsset extends EntityPathBase<Asset> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -481279559L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant asset. */
	public static final QAsset asset = new QAsset("asset");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category asset. */
	public final StringPath categoryAsset = createString("categoryAsset");

	/** The code article. */
	public final StringPath codeArticle = createString("codeArticle");

	/** The date debut. */
	public final DateTimePath<java.util.Date> dateDebut =
			createDateTime("dateDebut", java.util.Date.class);

	/** The date fin. */
	public final DateTimePath<java.util.Date> dateFin =
			createDateTime("dateFin", java.util.Date.class);

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The prix unitaire. */
	public final NumberPath<java.math.BigDecimal> prixUnitaire =
			createNumber("prixUnitaire", java.math.BigDecimal.class);

	/** The promotion. */
	public final StringPath promotion = createString("promotion");

	/** The promotion end date. */
	public final DateTimePath<java.util.Date> promotionEndDate =
			createDateTime("promotionEndDate", java.util.Date.class);

	/** The promotion start date. */
	public final DateTimePath<java.util.Date> promotionStartDate =
			createDateTime("promotionStartDate", java.util.Date.class);

	/** The remise article. */
	public final NumberPath<java.math.BigDecimal> remiseArticle =
			createNumber("remiseArticle", java.math.BigDecimal.class);

	/** The reserved stock. */
	public final StringPath reservedStock = createString("reservedStock");

	/** The sub category asset. */
	public final StringPath subCategoryAsset = createString("subCategoryAsset");

	/** The supplier. */
	public final QSupplier supplier;

	/** The supplier name. */
	public final StringPath supplierName = createString("supplierName");

	/** The tva. */
	public final NumberPath<Long> tva = createNumber("tva", Long.class);

	/** The type asset. */
	public final StringPath typeAsset = createString("typeAsset");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q asset.
	 *
	 * @param variable the variable
	 */
	public QAsset(String variable) {

		this(Asset.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q asset.
	 *
	 * @param path the path
	 */
	public QAsset(Path<? extends Asset> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q asset.
	 *
	 * @param metadata the metadata
	 */
	public QAsset(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q asset.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAsset(PathMetadata metadata, PathInits inits) {

		this(Asset.class, metadata, inits);
	}

	/**
	 * Instantiates a new q asset.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAsset(Class<? extends Asset> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.supplier =
				inits.isInitialized("supplier") ? new QSupplier(forProperty("supplier")) : null;
	}

}
