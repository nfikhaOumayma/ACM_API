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
 * QAcmDocuments is a Querydsl query type for AcmDocuments.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmDocuments extends EntityPathBase<AcmDocuments> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 393317252L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmDocuments. */
	public static final QAcmDocuments acmDocuments = new QAcmDocuments("acmDocuments");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The account number extern. */
	public final StringPath accountNumberExtern = createString("accountNumberExtern");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The auteur. */
	public final StringPath auteur = createString("auteur");

	/** The category. */
	public final StringPath category = createString("category");

	/** The collection instance id. */
	public final NumberPath<Long> collectionInstanceId =
			createNumber("collectionInstanceId", Long.class);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date creation. */
	public final DateTimePath<java.util.Date> dateCreation =
			createDateTime("dateCreation", java.util.Date.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The document index. */
	public final NumberPath<Integer> documentIndex = createNumber("documentIndex", Integer.class);

	/** The element id. */
	public final NumberPath<Long> elementId = createNumber("elementId", Long.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The expenses id. */
	public final NumberPath<Long> expensesId = createNumber("expensesId", Long.class);

	/** The id customer. */
	public final NumberPath<Long> idCustomer = createNumber("idCustomer", Long.class);

	/** The id document. */
	public final NumberPath<Long> idDocument = createNumber("idDocument", Long.class);

	/** The id document GED. */
	public final StringPath idDocumentGED = createString("idDocumentGED");

	/** The id ib document. */
	public final NumberPath<Long> idIbDocument = createNumber("idIbDocument", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The item instance id. */
	public final NumberPath<Long> itemInstanceId = createNumber("itemInstanceId", Long.class);

	/** The loan id. */
	public final NumberPath<Long> loanId = createNumber("loanId", Long.class);

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/** The name. */
	public final StringPath name = createString("name");

	/** The setting document type. */
	public final QSettingDocumentType settingDocumentType;

	/** The titre. */
	public final StringPath titre = createString("titre");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm documents.
	 *
	 * @param variable the variable
	 */
	public QAcmDocuments(String variable) {

		this(AcmDocuments.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm documents.
	 *
	 * @param path the path
	 */
	public QAcmDocuments(Path<? extends AcmDocuments> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm documents.
	 *
	 * @param metadata the metadata
	 */
	public QAcmDocuments(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm documents.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmDocuments(PathMetadata metadata, PathInits inits) {

		this(AcmDocuments.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm documents.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmDocuments(Class<? extends AcmDocuments> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.settingDocumentType = inits.isInitialized("settingDocumentType")
				? new QSettingDocumentType(forProperty("settingDocumentType"))
				: null;
	}

}
