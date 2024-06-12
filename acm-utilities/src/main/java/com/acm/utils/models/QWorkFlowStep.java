package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.ListPath;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QWorkFlowStep is a Querydsl query type for WorkFlowStep.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QWorkFlowStep extends EntityPathBase<WorkFlowStep> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1184490594L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant workFlowStep. */
	public static final QWorkFlowStep workFlowStep = new QWorkFlowStep("workFlowStep");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acceptation condition. */
	public final StringPath acceptationCondition = createString("acceptationCondition");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The approval conditions. */
	public final BooleanPath approvalConditions = createBoolean("approvalConditions");

	/** The approvers. */
	public final SetPath<Groupe, QGroupe> approvers = this.<Groupe, QGroupe>createSet("approvers",
			Groupe.class, QGroupe.class, PathInits.DIRECT2);

	/** The automatic step. */
	public final BooleanPath automaticStep = createBoolean("automaticStep");

	/** The check fees. */
	public final BooleanPath checkFees = createBoolean("checkFees");

	/** The check meza card. */
	public final BooleanPath checkMezaCard = createBoolean("checkMezaCard");

	/** The code acm template sms. */
	public final StringPath codeAcmTemplateSms = createString("codeAcmTemplateSms");

	/** The code statut loan. */
	public final NumberPath<Long> codeStatutLoan = createNumber("codeStatutLoan", Long.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The documents. */
	public final SetPath<SettingDocumentProduct, QSettingDocumentProduct> documents =
			this.<SettingDocumentProduct, QSettingDocumentProduct>createSet("documents",
					SettingDocumentProduct.class, QSettingDocumentProduct.class, PathInits.DIRECT2);

	/** The document types. */
	public final SetPath<SettingDocumentType, QSettingDocumentType> documentTypes =
			this.<SettingDocumentType, QSettingDocumentType>createSet("documentTypes",
					SettingDocumentType.class, QSettingDocumentType.class, PathInits.DIRECT2);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The generation task. */
	public final BooleanPath generationTask = createBoolean("generationTask");

	/** The group code. */
	public final StringPath groupCode = createString("groupCode");

	/** The ib screen. */
	public final StringPath ibScreen = createString("ibScreen");

	/** The id work flow step. */
	public final NumberPath<Long> idWorkFlowStep = createNumber("idWorkFlowStep", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The journal entry types. */
	public final ListPath<SettingJournalEntryType, QSettingJournalEntryType> journalEntryTypes =
			this.<SettingJournalEntryType, QSettingJournalEntryType>createList("journalEntryTypes",
					SettingJournalEntryType.class, QSettingJournalEntryType.class,
					PathInits.DIRECT2);

	/** The list charge fees. */
	public final SetPath<SettingChargeFee, QSettingChargeFee> listChargeFees =
			this.<SettingChargeFee, QSettingChargeFee>createSet("listChargeFees",
					SettingChargeFee.class, QSettingChargeFee.class, PathInits.DIRECT2);

	/** The lst fees list value. */
	public final SetPath<SettingListValues, QSettingListValues> lstFeesListValue =
			this.<SettingListValues, QSettingListValues>createSet("lstFeesListValue",
					SettingListValues.class, QSettingListValues.class, PathInits.DIRECT2);

	/** The max amount. */
	public final NumberPath<java.math.BigDecimal> maxAmount =
			createNumber("maxAmount", java.math.BigDecimal.class);

	/** The max score accepted. */
	public final NumberPath<Long> maxScoreAccepted = createNumber("maxScoreAccepted", Long.class);

	/** The max score rejected. */
	public final NumberPath<Long> maxScoreRejected = createNumber("maxScoreRejected", Long.class);

	/** The min amount. */
	public final NumberPath<java.math.BigDecimal> minAmount =
			createNumber("minAmount", java.math.BigDecimal.class);

	/** The min score accepted. */
	public final NumberPath<Long> minScoreAccepted = createNumber("minScoreAccepted", Long.class);

	/** The min score rejected. */
	public final NumberPath<Long> minScoreRejected = createNumber("minScoreRejected", Long.class);

	/** The object id. */
	public final NumberPath<Long> objectId = createNumber("objectId", Long.class);

	/** The order. */
	public final NumberPath<Long> order = createNumber("order", Long.class);

	/** The participants. */
	public final SetPath<Groupe, QGroupe> participants = this.<Groupe, QGroupe>createSet(
			"participants", Groupe.class, QGroupe.class, PathInits.DIRECT2);

	/** The planing step. */
	public final QPlaningStep planingStep;

	/** The previous step. */
	public final StringPath previousStep = createString("previousStep");

	/** The process. */
	public final StringPath process = createString("process");

	/** The process version. */
	public final NumberPath<Long> processVersion = createNumber("processVersion", Long.class);

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The ready for disb. */
	public final BooleanPath readyForDisb = createBoolean("readyForDisb");

	/** The rejection condition. */
	public final StringPath rejectionCondition = createString("rejectionCondition");

	/** The screen. */
	public final StringPath screen = createString("screen");

	/** The screening component. */
	public final StringPath screeningComponent = createString("screeningComponent");

	/** The step name. */
	public final StringPath stepName = createString("stepName");

	/** The step type. */
	public final StringPath stepType = createString("stepType");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user group. */
	public final StringPath userGroup = createString("userGroup");

	/**
	 * Instantiates a new q work flow step.
	 *
	 * @param variable the variable
	 */
	public QWorkFlowStep(String variable) {

		this(WorkFlowStep.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q work flow step.
	 *
	 * @param path the path
	 */
	public QWorkFlowStep(Path<? extends WorkFlowStep> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q work flow step.
	 *
	 * @param metadata the metadata
	 */
	public QWorkFlowStep(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q work flow step.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QWorkFlowStep(PathMetadata metadata, PathInits inits) {

		this(WorkFlowStep.class, metadata, inits);
	}

	/**
	 * Instantiates a new q work flow step.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QWorkFlowStep(Class<? extends WorkFlowStep> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.planingStep =
				inits.isInitialized("planingStep") ? new QPlaningStep(forProperty("planingStep"))
						: null;
	}

}
