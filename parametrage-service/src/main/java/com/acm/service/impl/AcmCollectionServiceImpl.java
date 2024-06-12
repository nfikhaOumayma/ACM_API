/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.acm.client.CreditClient;
import com.acm.client.CrmClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CollectionSettingException;
import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmCollectionRepository;
import com.acm.service.AcmCollectionService;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.AcmTemplateSMSService;
import com.acm.service.CollectionInstanceService;
import com.acm.service.CollectionNoteService;
import com.acm.service.GroupeService;
import com.acm.service.SettingWorkFlowService;
import com.acm.service.SmsSenderService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmTemplateSMSDTO;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.ChargeFeeDTO;
import com.acm.utils.dtos.ChargeFeesDTO;
import com.acm.utils.dtos.CollectionInstanceDTO;
import com.acm.utils.dtos.CollectionNoteDTO;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.MessageDetailsDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.ResponseChargeFeeDTO;
import com.acm.utils.dtos.SettingChargeFeeDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.CollectionPaginationDTO;
import com.acm.utils.enums.CollectionStatus;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationStatut;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.AcmCollection;
import com.acm.utils.models.QAcmCollection;
import com.acm.utils.models.QCollectionInstance;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Projections;
import com.querydsl.jpa.JPAExpressions;
import com.querydsl.jpa.impl.JPAQueryFactory;

/**
 * The Class AcmCollectionServiceImpl.
 */
@Service
public class AcmCollectionServiceImpl implements AcmCollectionService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmCollectionServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The collection repository. */
	@Autowired
	private AcmCollectionRepository collectionRepository;
	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The setting work flow service. */
	@Autowired
	private SettingWorkFlowService settingWorkFlowService;

	/** The collection instance service. */
	@Autowired
	private CollectionInstanceService collectionInstanceService;

	/** The crm client. */
	@Autowired
	private CrmClient crmClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The sms sender service. */
	@Autowired
	private SmsSenderService smsSenderService;

	/** The acm template SMS service. */
	@Autowired
	private AcmTemplateSMSService acmTemplateSMSService;

	/** The collection note service. */
	@Autowired
	private CollectionNoteService collectionNoteService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The groupe service. */
	@Autowired
	private GroupeService groupeService;

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#find(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public List<AcmCollectionDTO> find(AcmCollectionDTO acmCollectionDTO,
			Boolean checkOnOwnerIsConnectedUser) {

		Preconditions.checkNotNull(acmCollectionDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// init QAcmCollection
		QAcmCollection qCollection = QAcmCollection.acmCollection;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		predicate = buildQuery(acmCollectionDTO, qCollection, checkOnOwnerIsConnectedUser);
		// QueryDSL using springDATA
		Iterable<AcmCollection> iterable = collectionRepository.findAll(predicate);
		List<AcmCollection> collections = new ArrayList<>();
		iterable.forEach(collections::add);
		logger.info("Find Collections :: {} Collections founded", collections.size());

		// mapping returned list
		List<AcmCollectionDTO> acmCollectionDTOs = new ArrayList<>();
		collections.forEach(collection -> acmCollectionDTOs
				.add(mapper.map(collection, AcmCollectionDTO.class)));

		return acmCollectionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#find()
	 */
	@Override
	public List<AcmCollectionDTO> find() {

		List<AcmCollection> collections = collectionRepository.findAll();
		// mapping returned list
		List<AcmCollectionDTO> acmCollectionDTOs = new ArrayList<>();
		collections.forEach(collection -> acmCollectionDTOs
				.add(mapper.map(collection, AcmCollectionDTO.class)));

		logger.info("Find All Collections : {} Collections founded", collections.size());
		return acmCollectionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public AcmCollectionDTO save(Long id, AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmCollectionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update collection  with ID = {}", id);
		AcmCollection oldCollection = collectionRepository.findById(id).orElse(null);
		// check if object is null
		if (oldCollection == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmCollection.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmCollection.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldCollection)
		oldCollection.setStatus(acmCollectionDTO.getStatus());
		oldCollection.setIdAcmCollectionStep(acmCollectionDTO.getIdAcmCollectionStep());
		oldCollection.setOwner(acmCollectionDTO.getOwner());
		oldCollection.setOwnerName(acmCollectionDTO.getOwnerName());
		oldCollection.setGroupOwner(acmCollectionDTO.getGroupOwner());
		oldCollection.setGroupOwnerName(acmCollectionDTO.getGroupOwnerName());
		oldCollection.setStatutWorkflow(acmCollectionDTO.getStatutWorkflow());
		oldCollection.setStatutLibelle(acmCollectionDTO.getStatutLibelle());
		oldCollection.setStatutLibelleDone(acmCollectionDTO.getStatutLibelleDone());
		CommonFunctions.mapperToUpdate(oldCollection, userClient, logger);
		AcmCollection newCollection = collectionRepository.save(oldCollection);
		AcmCollectionDTO newCollectionDTO = mapper.map(newCollection, AcmCollectionDTO.class);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmCollection.class.getSimpleName());
		return newCollectionDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#save(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public AcmCollectionDTO save(AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmCollectionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// CHECK DATA
		if (ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getAccountNumber())) {
			logger.warn("Failed to INSERT new row : check sended DATA");
			return null;
		}

		AcmCollection acmCollection = mapper.map(acmCollectionDTO, AcmCollection.class);
		CommonFunctions.mapperToSave(acmCollection, userClient, logger);
		AcmCollection newAcmCollection = collectionRepository.save(acmCollection);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollection.class.getSimpleName());
		return mapper.map(newAcmCollection, AcmCollectionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#saveAll(java.util.List)
	 */
	@Override
	public void saveAll(List<AcmCollection> acmCollections, String processWF) {

		try {
			// update customers in ACM
			// List<AcmCollection> acmCollectionList = collectionRepository.saveAll(acmCollections);

			List<AcmCollectionDTO> collectionDTOs = new ArrayList<>();
			acmCollections.forEach(collection -> collectionDTOs
					.add(mapper.map(collection, AcmCollectionDTO.class)));
			// create collection instance for new collections
			// update pending action for existing collections

			startCollectionProcess(collectionDTOs, processWF);

		}
		catch (Exception e) {
			logger.error("Error while saving collections in acm_db = {}", e.getMessage());
		}

		logger.info("All collections are updated and saved successfully");

	}

	/**
	 * Start collection process.
	 *
	 * @param collectionDTOs the collection DT os
	 * @param processWF the process WF
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public List<AcmCollectionDTO> startCollectionProcess(List<AcmCollectionDTO> collectionDTOs,
			String processWF) throws ResourcesNotFoundException {

		for (AcmCollectionDTO collection : collectionDTOs) {

			// case of new collection
			if (ACMValidationUtils.isNullOrEmpty(collection.getId())) {
				logger.info("Start Adding new collections = {}", collection.getAccountNumber());

				// get setting workflow by product id
				List<CollectionStepDTO> workFlowCollectionSteps =
						getCollectionWfSetting(collection, processWF);

				if (!ACMValidationUtils.isNullOrEmpty(workFlowCollectionSteps)) {
					CollectionStepDTO collectionStepDto = workFlowCollectionSteps.get(0);
					collection.setIdAcmCollectionStep(collectionStepDto.getIdCollectionStep());
					collection.setStatutWorkflow(collectionStepDto.getStep_tab());
					collection.setStatutLibelle(collectionStepDto.getStepName());
					// Even for Done Status : put the first Status label to avoid empty filtering
					collection.setStatutLibelleDone(collectionStepDto.getStepName());

					// Calculate Available Date
					Calendar calendar = Calendar.getInstance();
					// if due date
					if (CommonConstants.DUE_DATE.equals(collectionStepDto.getAfterDate())) {
						calendar.setTime(collection.getFirstUnpaidInstallment());
						calendar.add(Calendar.DAY_OF_MONTH, collectionStepDto.getStartDate());
					}
					// after prev action completed date
					else if (CommonConstants.PREVIOUS_ACTION_COMPLETE_DATE
							.equals(collectionStepDto.getAfterDate())) {
						calendar.setTime(new Date());
						calendar.add(Calendar.DAY_OF_MONTH, collectionStepDto.getStartDate());

					}
					collection.setAvailableDate(calendar.getTime());

					AcmCollectionDTO newCollectionDTO = save(collection);
					// create workflow process instance for the new collection
					List<CollectionInstanceDTO> newCollectionInstanceDTOs =
							initProcessWorkflowCollection(newCollectionDTO,
									workFlowCollectionSteps);
					// assign collection
					AcmCollectionDTO acmCollectionForAssign =
							assignCollection(newCollectionInstanceDTOs, newCollectionDTO, "CREATE",
									workFlowCollectionSteps);

					AcmCollection acmCollection =
							mapper.map(acmCollectionForAssign, AcmCollection.class);
					collectionRepository.save(acmCollection);

					createTaskCalendar(newCollectionDTO, newCollectionInstanceDTOs);

				}
				else {
					logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
							CollectionStepDTO.class.getSimpleName());
				}

			}
			else {
				logger.info("Start Updating collections = {}", collection.getAccountNumber());

				NotifCollection(collection);
				// update collection
				AcmCollection acmCollection = mapper.map(collection, AcmCollection.class);
				// mapper to update collections
				CommonFunctions.mapperToUpdate(acmCollection, userClient, logger);
				// save collection
				collectionRepository.save(acmCollection);

			}
		}
		return collectionDTOs;
	}

	/**
	 * Generate collection wf instance.
	 *
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @param process the process
	 * @return the collection wf setting
	 */
	public List<CollectionStepDTO> getCollectionWfSetting(AcmCollectionDTO acmCollectionDTO,
			String process) {

		CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
		collectionStepDTO.setProductId(acmCollectionDTO.getProductId());
		collectionStepDTO.setProcess(process);
		collectionStepDTO.setEnabled(Boolean.TRUE);
		try {
			// find workflow steps by product id
			List<CollectionStepDTO> workFlowCollectionSteps =
					settingWorkFlowService.findCollectionStepsByProduct(collectionStepDTO);
			// sort the list by order
			return workFlowCollectionSteps.stream().sorted().collect(Collectors.toList());
		}
		catch (CollectionSettingException e) {
			logger.error("Error find steps with productId equal NULL");
		}

		return null;
	}

	/**
	 * Inits the process workflow collection.
	 *
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @param wfCollectionStepsDtos the wf collection steps dtos
	 * @return the list
	 */
	private List<CollectionInstanceDTO> initProcessWorkflowCollection(
			AcmCollectionDTO acmCollectionDTO, List<CollectionStepDTO> wfCollectionStepsDtos) {

		List<CollectionInstanceDTO> newCollectionInstanceList =
				new ArrayList<CollectionInstanceDTO>();

		int i = 0;
		for (CollectionStepDTO collectionstepDTO : wfCollectionStepsDtos) {

			CollectionInstanceDTO newCollectionInstanceDTO = collectionInstanceService
					.save(new CollectionInstanceDTO(acmCollectionDTO.getId(),
							collectionstepDTO.getIdCollectionStep().intValue(),
							collectionstepDTO.getStepName(), "", collectionstepDTO.getProcess(),
							collectionstepDTO.getScreen(), null, i, collectionstepDTO.getEnabled(),
							collectionstepDTO.getStartDate(), collectionstepDTO.getAfterDate(),
							collectionstepDTO.getStepName()));

			newCollectionInstanceList.add(newCollectionInstanceDTO);

			if (!ACMValidationUtils.isNullOrEmpty(collectionstepDTO.getDocuments())) {
				for (SettingDocumentProductDTO SettingDocDTO : collectionstepDTO.getDocuments()) {
					AcmDocumentsDTO newDocumentForCollStep = new AcmDocumentsDTO();
					newDocumentForCollStep
							.setCollectionInstanceId(newCollectionInstanceDTO.getId());
					newDocumentForCollStep.setDescription(
							SettingDocDTO.getSettingDocumentTypeDTO().getDescription());
					newDocumentForCollStep
							.setTitre(SettingDocDTO.getSettingDocumentTypeDTO().getLibelle());
					newDocumentForCollStep.setMandatory(SettingDocDTO.getMandatory());
					newDocumentForCollStep.setEnabled(SettingDocDTO.getEnabled());
					newDocumentForCollStep.setInsertBy("Batch");
					newDocumentForCollStep
							.setSettingDocumentTypeDTO(SettingDocDTO.getSettingDocumentTypeDTO());
					creditClient.createAcmDocuments(newDocumentForCollStep);

				}
			}
			i++;
		}
		logger.debug("{}", newCollectionInstanceList);
		logger.info(
				"Collection process for collection with AccountNumber = [{}] was successfully inserted :: DONE",
				acmCollectionDTO.getAccountNumber());
		return newCollectionInstanceList;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#closeCollections()
	 */
	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public int closeCollections() {

		int updatedCollections = 0;
		try {
			updatedCollections = collectionRepository.closeCollections(
					CollectionStatus.CLOSED.statusId(), CollectionStatus.ACTIVE.statusId());
			logger.info("Closed collections updated successfully : {} ", updatedCollections);
		}
		catch (Exception e) {
			logger.error("Error while updating status Close in acm_collection table = {}",
					e.getMessage());
		}
		return updatedCollections;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#closeCollectionTasks()
	 */
	@Override
	public void closeCollectionTasks() {

		try {
			collectionRepository.closeTasksForClosedCollections(
					ACMConstantWorkflowStatuts.TASK_STATUS_CLOSED,
					CollectionStatus.CLOSED.statusId(), ACMConstantWorkflowStatuts.TASK_STATUS_NEW);
			logger.info("Events related to closed collections are updated successfully");
		}
		catch (Exception e) {
			logger.error("Error while updating status Close in acm_collection_event table = {}",
					e.getMessage());
		}

	}

	/**
	 * Creates the task calendar.
	 *
	 * @author idridi
	 * @param acmCollectionDTO the acm collection DTO
	 * @param collectionInstanceDTOs the collection instance DT os
	 */
	public void createTaskCalendar(AcmCollectionDTO acmCollectionDTO,
			List<CollectionInstanceDTO> collectionInstanceDTOs) {

		CalendarEventDTO newCalendarTask = new CalendarEventDTO();

		try {
			List<CollectionStepDTO> collectionStepDTO = new ArrayList<>();
			Date currentDate = new Date();
			CollectionInstanceDTO collectionInstanceDTO = collectionInstanceDTOs.stream()
					.filter(instance -> instance.getIdAcmCollectionStep()
							.equals(acmCollectionDTO.getIdAcmCollectionStep().intValue()))
					.findFirst().orElse(null);
			// Get setting collection step
			CollectionStepDTO collectionStepDTOParam = new CollectionStepDTO();

			if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTO) && !ACMValidationUtils
					.isNullOrEmpty(collectionInstanceDTO.getIdAcmCollectionStep())) {
				collectionStepDTOParam.setIdCollectionStep(
						collectionInstanceDTO.getIdAcmCollectionStep().longValue());
				collectionStepDTO =
						settingWorkFlowService.findSettingCollection(collectionStepDTOParam);
			}

			// check if generation task is active
			if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTO)
					&& !ACMValidationUtils.isNullOrEmpty(collectionStepDTO)
					&& Boolean.TRUE.equals(collectionStepDTO.get(0).getGenerationTask())) {
				Calendar calendar = Calendar.getInstance();

				// if due date : create task with start date = date of first unpaid installment + nb
				// start date (setting)
				if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTO)
						&& collectionInstanceDTO.getAfterDate().equals(CommonConstants.DUE_DATE)) {

					// date first unpaid installment + nb start date (setting)
					calendar.setTime(acmCollectionDTO.getFirstUnpaidInstallment());
					calendar.add(Calendar.DAY_OF_MONTH, collectionInstanceDTO.getStartDate());

				}
				// if based on prev action : start date of the task : date action complete of prev
				// action + nb start date (setting)
				else if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTO)
						&& collectionInstanceDTO.getAfterDate()
								.equals(CommonConstants.PREVIOUS_ACTION_COMPLETE_DATE)) {
					// date first unpaid installment + nb start date (setting)
					calendar.setTime(new Date());
					calendar.add(Calendar.DAY_OF_MONTH, collectionInstanceDTO.getStartDate());
				}
				// fill task data
				// Check if the date of the new task is less than the system date
				if (calendar.getTime().before(currentDate)) {
					newCalendarTask.setDateDebut(currentDate);
					newCalendarTask.setDateFin(currentDate);
					calendar.setTime(currentDate);
				}
				else {
					newCalendarTask.setDateDebut(calendar.getTime());
					newCalendarTask.setDateFin(calendar.getTime());
				}

				newCalendarTask.setLibelleEvent(collectionInstanceDTO.getStepName());
				newCalendarTask.setDescription(collectionInstanceDTO.getStepName());
				newCalendarTask.setCustomerName(acmCollectionDTO.getCustomerName());
				newCalendarTask.setIdCustomerExtern(acmCollectionDTO.getCustomerIdExtern());
				newCalendarTask.setTypeEvent("task");
				newCalendarTask.setIdLoanExtern(acmCollectionDTO.getIdLoanExtern());
				newCalendarTask
						.setCustomerNumber(acmCollectionDTO.getCustomerIdExtern().toString());

				newCalendarTask.setIdCollection(acmCollectionDTO.getId());
				newCalendarTask.setUsername(acmCollectionDTO.getOwner());
				newCalendarTask.setCategory(acmCollectionDTO.getCollectionType());
				if (ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getOwner())
						&& !ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getGroupOwner())) {
					// update collection data
					// find group by code group
					GroupeDTO groupeDTO =
							groupeService.findByCode(acmCollectionDTO.getGroupOwner());
					// get the list of users that have loanBranch in their accessBranch
					List<UserDTO> userDTOParam =
							userClient.getUsersWithLoanBranchInTheirAccessBranches(
									acmCollectionDTO.getBranchId().intValue(),
									groupeDTO.getUserDTOs());
					// init username
					List<String> usernames = new ArrayList<>();
					// init username
					userDTOParam.forEach(user -> usernames.add(user.getLogin()));

					// create list of tasks
					List<CalendarEventDTO> newListCalendarTask = new ArrayList<CalendarEventDTO>();
					// list of usernames (group members)
					usernames.forEach(username -> {
						CalendarEventDTO newTask = new CalendarEventDTO();
						newTask.setDateDebut(calendar.getTime());
						newTask.setDateFin(calendar.getTime());
						newTask.setLibelleEvent(collectionInstanceDTO.getStepName());
						newTask.setDescription(collectionInstanceDTO.getStepName());
						newTask.setCustomerName(acmCollectionDTO.getCustomerName());
						newTask.setTypeEvent("task");
						newTask.setIdLoanExtern(acmCollectionDTO.getIdLoanExtern());
						newTask.setIdCollection(acmCollectionDTO.getId());
						newTask.setCategory(acmCollectionDTO.getCollectionType());
						newTask.setUsername(username);
						newCalendarTask.setCustomerNumber(
								acmCollectionDTO.getCustomerIdExtern().toString());

						newListCalendarTask.add(newTask);
					});
					// create tasks for all group members
					crmClient.saveAll(newListCalendarTask);
				}
				else {
					// case of one owner
					crmClient.create(newCalendarTask);
				}
			}

		}
		catch (Exception e) {
			logger.error("Error while Creating Task for Collection = {}", e.getMessage());
		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#find(com.acm.utils.dtos.pagination.
	 * CollectionPaginationDTO)
	 */
	@Override
	public CollectionPaginationDTO find(CollectionPaginationDTO collectionPaginationDTO) {

		Preconditions.checkNotNull(collectionPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(collectionPaginationDTO.getPageNumber())) {
			collectionPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(collectionPaginationDTO.getPageSize())) {
			collectionPaginationDTO.setPageSize(10);
		}
		// setting default data
		collectionPaginationDTO.setResultsCollections(new ArrayList<>());
		// setting default totals pages
		collectionPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		collectionPaginationDTO.setTotalPages(0);
		// init QAcmCollection
		QAcmCollection qCollection = QAcmCollection.acmCollection;
		// build Predicate using given params
		BooleanBuilder predicate =
				buildQuery(collectionPaginationDTO.getParams(), qCollection, Boolean.FALSE);

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(collectionPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(collectionPaginationDTO.getSortField())) {
			pageable = PageRequest.of(collectionPaginationDTO.getPageNumber(),
					collectionPaginationDTO.getPageSize(), Sort.Direction.ASC,
					collectionPaginationDTO.getSortField());
		}
		else if ("-1".equals(collectionPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(collectionPaginationDTO.getSortField())) {
			pageable = PageRequest.of(collectionPaginationDTO.getPageNumber(),
					collectionPaginationDTO.getPageSize(), Sort.Direction.DESC,
					collectionPaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(collectionPaginationDTO.getPageNumber(),
					collectionPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<AcmCollection> pagedResult = collectionRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<AcmCollection> collections = pagedResult.getContent();
			logger.info("{} : Collection was found (PageNumber = {} / PageSize = {} )",
					collections.size(), collectionPaginationDTO.getPageNumber(),
					collectionPaginationDTO.getPageSize());
			List<AcmCollectionDTO> collectionDTOs = new ArrayList<>();
			collections.forEach(collection -> collectionDTOs
					.add(mapper.map(collection, AcmCollectionDTO.class)));
			// setting data
			collectionPaginationDTO.setResultsCollections(collectionDTOs);
			// setting totals pages
			collectionPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			collectionPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return collectionPaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @param qCollection the q collection
	 * @param checkOnOwnerIsConnectedUser the check on owner is connected user
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmCollectionDTO acmCollectionDTO, QAcmCollection qCollection,
			Boolean checkOnOwnerIsConnectedUser) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// init QCollectionInstance
		QCollectionInstance qCollectionInstance = QCollectionInstance.collectionInstance;
		// find only enabled data
		predicate.and(qCollection.enabled.eq(Boolean.TRUE));

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		try {
			boolean isAdmin = acmEnvironnementService
					.checkAthorisationConnectedUser(CommonConstants.AUTHORIZED_GROUPS);

			if (!isAdmin) {
				if (CommonConstants.UNASSIGNED.equalsIgnoreCase(acmCollectionDTO.getOwner())) {
					// find collections with an empty owner
					GroupeDTO groupeDTO = userDTO.getGroupes().iterator().next();
					// check if the group of user connected is not empty
					if (!ACMValidationUtils.isNullOrEmpty(groupeDTO)
							&& !ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
						// find only collections without owner
						predicate.and(qCollection.owner.isNull());
						// find collections by group owner of connected user
						predicate.and(qCollection.groupOwner.eq(groupeDTO.getCode()));
					}
				}
				else {
					// if user logged in is responsible then get collaborators, otherwise
					// only returns user
					// logged in
					List<String> responsibleAndCollaborators = new ArrayList<>();
					List<UserDTO> userDTOs = userClient.findUsers();
					userDTOs.forEach(user -> {
						if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
							responsibleAndCollaborators.add(user.getLogin());
						}
					});
					// Find Collections by owner for user logged in
					// Also gets the Collections of Collaborators if user logged in is
					// Responsable
					predicate.and(qCollection.owner.in(responsibleAndCollaborators));
					// find collections if users have performed an action on a collection
					// from
					// collectionInstances even if they are not the owner
					BooleanBuilder subPredicate = new BooleanBuilder();
					subPredicate
							.and(qCollection.id.in(JPAExpressions.selectFrom(qCollectionInstance)
									.select(qCollectionInstance.collection.id)
									.where(qCollectionInstance.actionUser
											.in(responsibleAndCollaborators)
											.and(qCollectionInstance.enabled.eq(Boolean.TRUE)))));

					predicate.or(subPredicate);

					// find collections by Access Branches for connected user if user
					// category is MANAGMENT
					if (userDTO.getCategory() != null
							&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
							&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
						int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
								.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
						List<Long> listBranchIds = new ArrayList<>(arrayBranchIds.length);
						for (int i : arrayBranchIds) {
							listBranchIds.add(Long.valueOf(i));
						}
						// setting predicate to find by given branch Id
						predicate.and(qCollection.branchId.in(listBranchIds));
					}
				}
			}

		}
		catch (ResourcesNotFoundException e) {
			logger.info("AUTHORIZED_GROUPS environement key not found");
			e.printStackTrace();
		}

		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getGroupOwnerName())) {
			predicate.and(qCollection.groupOwnerName.like(acmCollectionDTO.getTypeCustomer()));
		}

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getId())) {
			predicate.and(qCollection.id.eq(acmCollectionDTO.getId()));
		}
		// find by typeCustomer
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getTypeCustomer())) {
			predicate.and(qCollection.typeCustomer.like(acmCollectionDTO.getTypeCustomer()));
		}
		// find by accountNumber
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getAccountNumber())) {
			predicate.and(qCollection.accountNumber
					.like("%" + acmCollectionDTO.getAccountNumber() + "%"));

		}
		// find by productDescription
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getProductDescription())) {
			predicate.and(qCollection.productDescription
					.like("%" + acmCollectionDTO.getProductDescription() + "%"));
		}
		// FIND BY CustomerName
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getCustomerName())) {
			predicate.and(
					qCollection.customerName.like("%" + acmCollectionDTO.getCustomerName() + "%"));

		}
		// find by CustomerIdExtern
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getCustomerIdExtern())) {
			predicate.and(qCollection.customerIdExtern.eq(acmCollectionDTO.getCustomerIdExtern()));
		}
		// find by branchDescrption
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getBranchDescription())) {
			predicate.and(qCollection.branchDescription
					.like("%" + acmCollectionDTO.getBranchDescription() + "%"));

		}
		// find by loan amount
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getAmount())) {
			predicate.and(qCollection.amount.eq(acmCollectionDTO.getAmount()));
		}

		// find by loanOfficer
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getLoanOfficer())) {
			predicate.and(
					qCollection.loanOfficer.like("%" + acmCollectionDTO.getLoanOfficer() + "%"));
		}

		// find by idAcmCollectionStep
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getIdAcmCollectionStep())) {
			predicate.and(
					qCollection.idAcmCollectionStep.eq(acmCollectionDTO.getIdAcmCollectionStep()));
		}

		// find by statut Libelle
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getStatutLibelle())) {
			Date current = new Date();

			BooleanBuilder predicateS1 = new BooleanBuilder();
			predicateS1.and(qCollection.statutLibelle.eq(acmCollectionDTO.getStatutLibelle()));
			predicateS1.and(qCollection.availableDate.before(current));

			BooleanBuilder predicateS2 = new BooleanBuilder();
			predicateS2.and(qCollection.statutLibelleDone.eq(acmCollectionDTO.getStatutLibelle()));
			predicateS2.andNot(qCollection.availableDate.before(current));

			predicate.and(predicateS1.or(predicateS2));
		}

		// find by loan unpaid amount
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getUnpaidAmount())) {
			predicate.and(qCollection.unpaidAmount.eq(acmCollectionDTO.getUnpaidAmount()));
		}

		// find by status
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getStatus())) {
			predicate.and(qCollection.status.eq(acmCollectionDTO.getStatus()));
		}

		// find by Statut Workflow
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getStatutWorkflow())) {
			predicate.and(qCollection.statutWorkflow.eq(acmCollectionDTO.getStatutWorkflow()));
		}

		// find by late days
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getLateDays())) {
			predicate.and(qCollection.lateDays.eq(acmCollectionDTO.getLateDays()));
		}

		// find by type collection
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getCollectionType())) {
			predicate.and(qCollection.collectionType.eq(acmCollectionDTO.getCollectionType()));
		}

		// find by id parent collection
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getIdParentCollection())) {
			predicate.and(
					qCollection.idParentCollection.eq(acmCollectionDTO.getIdParentCollection()));
		}

		// find by number of unpaid installment
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getNumberOfUnpaidInstallment())) {
			predicate.and(qCollection.numberOfUnpaidInstallment
					.eq(acmCollectionDTO.getNumberOfUnpaidInstallment()));
		}
		// find by creation date
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getDateInsertion())) {
			Timestamp StartTimesTamp =
					DateUtil.dateToDateTime(acmCollectionDTO.getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp =
					DateUtil.dateToDateTime(acmCollectionDTO.getDateInsertion(), "23:59:59");
			predicate.and(qCollection.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}
		// find by FirstUnpaidInstallment
		if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTO.getFirstUnpaidInstallment())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(acmCollectionDTO.getFirstUnpaidInstallment(), "00:00:01");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(acmCollectionDTO.getFirstUnpaidInstallment(), "23:59:59");
			predicate.and(qCollection.firstUnpaidInstallment.between(StartTimesTamp, EndTimesTamp));
		}

		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmCollectionService#loadFilterBranch(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public List<AcmCollectionDTO> loadFilterBranch(AcmCollectionDTO acmCollectionDTO) {

		// init QAcmCollection
		QAcmCollection qCollection = QAcmCollection.acmCollection;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(acmCollectionDTO, qCollection, Boolean.FALSE);
		// SELECT distinct branches
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<AcmCollection> collections = queryFactory
				.select(Projections.bean(AcmCollection.class, qCollection.branchDescription))
				.distinct().from(qCollection).where(predicate).fetch();

		// mapping && returning data
		List<AcmCollectionDTO> collectionDTOs = new ArrayList<>();
		collections.forEach(
				collection -> collectionDTOs.add(mapper.map(collection, AcmCollectionDTO.class)));
		logger.info("{} : branch to use in Filter dashboard was founded", collectionDTOs.size());
		return collectionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmCollectionService#loadFilterStatus(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public List<AcmCollectionDTO> loadFilterStatus(AcmCollectionDTO acmCollectionDTO) {

		// init QAcmCollection
		QAcmCollection qCollection = QAcmCollection.acmCollection;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(acmCollectionDTO, qCollection, Boolean.FALSE);

		// find only current status
		BooleanBuilder predicate1 = new BooleanBuilder();
		predicate1.and(qCollection.statutLibelle.isNotNull());
		predicate1.and(qCollection.availableDate.before(new Date()));
		predicate1.andAnyOf(predicate);

		// find only prev status
		BooleanBuilder predicate2 = new BooleanBuilder();
		predicate2.and(qCollection.statutLibelleDone.isNotNull());
		predicate2.andNot(qCollection.availableDate.before(new Date()));
		predicate2.andAnyOf(predicate);
		// SELECT distinct status
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<AcmCollection> collections = new ArrayList<>();
		collections.addAll(queryFactory
				.select(Projections.bean(AcmCollection.class, qCollection.statutLibelle)).distinct()
				.from(qCollection).where(predicate1).fetch());

		collections.addAll(queryFactory
				.select(Projections.bean(AcmCollection.class, qCollection.statutLibelleDone))
				.distinct().from(qCollection).where(predicate2).fetch());

		// mapping && returning data
		List<AcmCollectionDTO> collectionDTOs = new ArrayList<>();
		collections.forEach(
				collection -> collectionDTOs.add(mapper.map(collection, AcmCollectionDTO.class)));
		logger.info("{} : branch to use in Filter dashboard was founded", collectionDTOs.size());
		return collectionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmCollectionService#loadFilterProduct(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public List<AcmCollectionDTO> loadFilterProduct(AcmCollectionDTO acmCollectionDTO) {

		// init QAcmCollection
		QAcmCollection qCollection = QAcmCollection.acmCollection;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(acmCollectionDTO, qCollection, Boolean.FALSE);

		// SELECT distinct productDescription
		JPAQueryFactory queryFactory = new JPAQueryFactory(entityManager);
		List<AcmCollection> collections = queryFactory
				.select(Projections.bean(AcmCollection.class, qCollection.productDescription))
				.distinct().from(qCollection).where(predicate).fetch();

		// mapping && returning data
		List<AcmCollectionDTO> collectionDTOs = new ArrayList<>();
		collections.forEach(
				collection -> collectionDTOs.add(mapper.map(collection, AcmCollectionDTO.class)));
		logger.info("{} : Products to use in Filter dashboard was founded", collectionDTOs.size());
		return collectionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#completeAction(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public AcmCollectionDTO completeAction(AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException, ApiAbacusException {

		Preconditions.checkNotNull(acmCollectionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(acmCollectionDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// Init initial/old pending action
		Long oldPendingAction = acmCollectionDTO.getIdAcmCollectionStep();
		// find collection instance
		CollectionInstanceDTO parmCollectionInstanceDTO = new CollectionInstanceDTO();
		parmCollectionInstanceDTO.setIdCollection(acmCollectionDTO.getId());
		List<CollectionInstanceDTO> collectionInstanceDTOs =
				collectionInstanceService.find(parmCollectionInstanceDTO);
		// sort collection instance by order process
		Collections.sort(collectionInstanceDTOs, (step1, step2) -> {
			return step1.getOrderEtapeProcess() - step2.getOrderEtapeProcess();
		});

		// set the prev status libelle on the Done field whatever the next step
		acmCollectionDTO.setStatutLibelleDone(acmCollectionDTO.getStatutLibelle());
		// check if is the last step last step
		if (acmCollectionDTO.getIdAcmCollectionStep().equals(collectionInstanceDTOs
				.get(collectionInstanceDTOs.size() - 1).getIdAcmCollectionStep().longValue())) {
			// Workflow collection is completed
			acmCollectionDTO.setStatus(CollectionStatus.COMPLETED.statusId());
		}
		else {
			// assign collection
			AcmCollectionDTO acmCollectionForAssign =
					assignCollection(collectionInstanceDTOs, acmCollectionDTO, "UPDATE", null);
			acmCollectionDTO.setOwner(acmCollectionForAssign.getOwner());
			acmCollectionDTO.setOwnerName(acmCollectionForAssign.getOwnerName());
			acmCollectionDTO.setGroupOwner(acmCollectionForAssign.getGroupOwner());
			acmCollectionDTO.setGroupOwnerName(acmCollectionForAssign.getGroupOwnerName());
			// find the next step
			CollectionInstanceDTO collectionInstanceDTO =
					collectionInstanceDTOs.stream()
							.filter(c -> c.getIdAcmCollectionStep() == acmCollectionDTO
									.getIdAcmCollectionStep().intValue() + 1)
							.findFirst().orElse(null);
			// find setting next step
			CollectionStepDTO acmCollectionSettingStepDTO = new CollectionStepDTO();
			acmCollectionSettingStepDTO.setIdCollectionStep(
					collectionInstanceDTO.getIdAcmCollectionStep().longValue());
			acmCollectionSettingStepDTO = settingWorkFlowService
					.findSettingCollection(acmCollectionSettingStepDTO).get(0);

			if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTO)) {
				Calendar calendar = Calendar.getInstance();
				// if due date
				if (CommonConstants.DUE_DATE.equals(collectionInstanceDTO.getAfterDate())) {
					calendar.setTime(acmCollectionDTO.getFirstUnpaidInstallment());
					calendar.add(Calendar.DAY_OF_MONTH, collectionInstanceDTO.getStartDate());
					acmCollectionDTO.setAvailableDate(calendar.getTime());
				}
				// after prev action completed date
				else if (CommonConstants.PREVIOUS_ACTION_COMPLETE_DATE
						.equals(collectionInstanceDTO.getAfterDate())) {
					calendar.setTime(new Date());
					calendar.add(Calendar.DAY_OF_MONTH, collectionInstanceDTO.getStartDate());
					acmCollectionDTO.setAvailableDate(calendar.getTime());
				}
				acmCollectionDTO.setIdAcmCollectionStep(
						collectionInstanceDTO.getIdAcmCollectionStep().longValue());

				acmCollectionDTO.setStatutWorkflow(acmCollectionSettingStepDTO.getStep_tab());

				// set the current status libelle from instance
				acmCollectionDTO.setStatutLibelle(collectionInstanceDTO.getLibelle());
				// updatedCollection = save(acmCollection.getId(), acmCollection);
			}
		}
		// get actual collection instance step
		CollectionStepDTO actualCollectionStepDTO = new CollectionStepDTO();
		actualCollectionStepDTO.setIdCollectionStep(oldPendingAction);
		actualCollectionStepDTO =
				settingWorkFlowService.findSettingCollection(actualCollectionStepDTO).get(0);
		final CollectionStepDTO finalCollectionstep = actualCollectionStepDTO;
		CollectionInstanceDTO curentCollectionInstanceDTO = collectionInstanceDTOs.stream()
				.filter(instance -> instance.getIdAcmCollectionStep() == finalCollectionstep
						.getIdCollectionStep().intValue())
				.findFirst().orElse(null);

		if (!ACMValidationUtils.isNullOrEmpty(actualCollectionStepDTO)
				&& !ACMValidationUtils.isNullOrEmpty(curentCollectionInstanceDTO)) {
			// Charge Fee
			if (actualCollectionStepDTO.getListChargeFees().size() > 0) {
				LoanDTO loanDTO = creditClient.findByIdExtern(acmCollectionDTO.getIdLoanExtern());
				ChargeFeesDTO chargeFeeDTOParam = new ChargeFeesDTO();
				chargeFeeDTOParam.setIdCollectionInstance(curentCollectionInstanceDTO.getId());
				chargeFeeDTOParam.setCharged(Boolean.FALSE);
				List<ChargeFeesDTO> feesToCharge = creditClient.findChargeFees(chargeFeeDTOParam);
				feesToCharge = addAutomaticFees(loanDTO, actualCollectionStepDTO,
						curentCollectionInstanceDTO, feesToCharge);

				if (!ACMValidationUtils.isNullOrEmpty(feesToCharge)) {
					try {
						ResponseChargeFeeDTO responseChargeFeeDTO = new ResponseChargeFeeDTO();
						responseChargeFeeDTO =
								transversClient.initializeChargeFee(loanDTO.getIdAccountExtern());
						responseChargeFeeDTO = setChargeFee(feesToCharge, responseChargeFeeDTO);
						logger.info("FEES CHARGED");
						transversClient.postChargeFees(responseChargeFeeDTO);
						creditClient.createAllChargeFees(feesToCharge);

					}
					catch (Exception ex) {
						throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
								"charge fee exception : " + ex.getMessage());
					}
					// add new note
					addCollectionNote(acmCollectionDTO, feesToCharge);
				}
			}
		}

		AcmCollection acmCollection = mapper.map(acmCollectionDTO, AcmCollection.class);
		// mapper to update collections
		CommonFunctions.mapperToUpdate(acmCollection, userClient, logger);
		// save collection
		AcmCollection newCollection = collectionRepository.save(acmCollection);

		AcmCollectionDTO updatedCollectionDTO = mapper.map(newCollection, AcmCollectionDTO.class);

		// close old task
		closeTask(updatedCollectionDTO);
		// check if the step last step
		if (!oldPendingAction.equals(collectionInstanceDTOs.get(collectionInstanceDTOs.size() - 1)
				.getIdAcmCollectionStep().longValue())) {
			// create task for the next step
			createTaskCalendar(updatedCollectionDTO, collectionInstanceDTOs);
		}
		// add new note
		CollectionNoteDTO collectionNoteDTO = new CollectionNoteDTO();
		collectionNoteDTO.setComment("Action Completed!");
		collectionNoteDTO.setAction(acmCollectionDTO.getStatutLibelleDone());
		collectionNoteDTO.setCollectionId(acmCollectionDTO.getId());
		collectionNoteService.save(collectionNoteDTO);

		// send notification to the owner of collection
		NotificationsDTO notificationsDTO =
				creditClient.create(new NotificationsDTO(acmCollectionDTO.getOwner(),
						NotificationCategory.COLLECTION.name(), NotificationType.INFO.name(),
						Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_NEW, "",
						acmCollectionDTO.getId()));
		logger.info("New Notification  [{}] has been inserted.", notificationsDTO);
		updatedCollectionDTO
				.setCollectionInstancesDtos(acmCollectionDTO.getCollectionInstancesDtos());

		// Send SMS
		Boolean serviceSMS = acmEnvironnementService.find("SERVICE_SMS").getEnabled();
		CustomerDTO customerDTO =
				creditClient.findCustomerIdExtern(acmCollectionDTO.getCustomerIdExtern()).get(0);
		CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
		collectionStepDTO
				.setIdCollectionStep(acmCollectionDTO.getIdAcmCollectionStep().longValue());
		collectionStepDTO.setProductId(acmCollectionDTO.getProductId());
		collectionStepDTO.setProcess(acmCollectionDTO.getCollectionType());

		try {
			CollectionStepDTO collectionStepDTO1 =
					settingWorkFlowService.findCollectionStepsByProduct(collectionStepDTO).get(0);
			if (collectionStepDTO1.getCodeAcmTemplateSms() != null) {

				AcmTemplateSMSDTO acmtemplateSMSDTO = new AcmTemplateSMSDTO();
				acmtemplateSMSDTO.setCodeSMSEvent(collectionStepDTO1.getCodeAcmTemplateSms());
				AcmTemplateSMSDTO acmtemplateSMSDTO1 =
						acmTemplateSMSService.findByCode(acmtemplateSMSDTO);
				if (!ACMValidationUtils.isNullOrEmpty(customerDTO.getTelephone1()) && serviceSMS) {
					try {
						MessageDetailsDTO messageDetailsDTO = new MessageDetailsDTO();
						messageDetailsDTO.setToSender(customerDTO.getTelephone1());

						// Utilisation d'une expression régulière pour identifier les balises
						// "${...}"
						Pattern pattern = Pattern.compile("\\$\\{([^}]+)\\}");
						Matcher matcher = pattern.matcher(acmtemplateSMSDTO1.getMessageBody());

						StringBuffer resultat = new StringBuffer();

						while (matcher.find()) {
							String nomEntite = matcher.group(1); // Capturer le nom de l'entité
							String valeurRemplacement =
									evaluerExpression(nomEntite, acmCollectionDTO);
							if (!valeurRemplacement.isEmpty()) {
								matcher.appendReplacement(resultat, valeurRemplacement);
							}

						}

						matcher.appendTail(resultat);
						String texteModifie = resultat.toString();

						messageDetailsDTO.setMessageBody(texteModifie);
						messageDetailsDTO.setCategory(acmtemplateSMSDTO1.getCategory());
						smsSenderService.saveSms(messageDetailsDTO);
						smsSenderService.sendSms(messageDetailsDTO.getToSender(),
								messageDetailsDTO.getMessageBody());
					}
					catch (Exception e) {
						logger.error(e.getMessage());
					}
				}
			}

		}
		catch (CollectionSettingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		// return dto
		return updatedCollectionDTO;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollectionService#evaluerExpression(java.lang.String,
	 * com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public String evaluerExpression(String expression, AcmCollectionDTO acmCollectionDTO) {

		// Divisez l'expression en entité et champ
		String[] elements = expression.split("\\.");
		if (elements.length == 2) {
			String nomEntite = elements[0];
			String nomChamp = elements[1];

			// Utilisez la réflexion pour appeler la méthode correspondante
			try {

				// Class dtoClass = loanDTO.getClass();
				if (nomEntite.equals("customer")) {
					CustomerDTO customer = creditClient
							.findCustomerIdExtern(acmCollectionDTO.getCustomerIdExtern()).get(0);
					Class dtoClass = customer.getClass();
					// Get the method with the specified name
					Method method = dtoClass.getMethod(
							"get" + nomChamp.substring(0, 1).toUpperCase() + nomChamp.substring(1));
					if (method.getReturnType() == Date.class) {
						Date dateValue = (Date) method.invoke(customer);

						// Formatez la date en "jj/mm/aaaa" (24/10/2023)
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
						String formattedDate = dateFormat.format(dateValue);

						// formattedDate contient la date formatée

						return formattedDate;
					}
					else {

						String valeurChampStr = String.valueOf(method.invoke(customer));
						return valeurChampStr;
					}
				}
				else if (nomEntite.equals("collection")) {
					Class dtoClass = acmCollectionDTO.getClass();
					// Get the method with the specified name
					Method method = dtoClass.getMethod(
							"get" + nomChamp.substring(0, 1).toUpperCase() + nomChamp.substring(1));
					if (method.getReturnType() == Date.class) {
						Date dateValue = (Date) method.invoke(acmCollectionDTO);

						// Formatez la date en "jj/mm/aaaa" (24/10/2023)
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
						String formattedDate = dateFormat.format(dateValue);

						// formattedDate contient la date formatée

						return formattedDate;
					}
					else {

						String valeurChampStr = String.valueOf(method.invoke(acmCollectionDTO));
						return valeurChampStr;
					}
				}
				else {
					LoanDTO loan = creditClient.findByIdExtern(acmCollectionDTO.getIdLoanExtern());

					Class dtoClass = loan.getClass();
					// Get the method with the specified name
					Method method = dtoClass.getMethod(
							"get" + nomChamp.substring(0, 1).toUpperCase() + nomChamp.substring(1));
					if (method.getReturnType() == Date.class) {
						Date dateValue = (Date) method.invoke(loan);

						// Formatez la date en "jj/mm/aaaa" (24/10/2023)
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
						String formattedDate = dateFormat.format(dateValue);

						// formattedDate contient la date formatée

						return formattedDate;
					}
					else {
						String value = String.valueOf(method.invoke(loan));
						return value;
					}
				}

			}

			catch (Exception e) {
				e.printStackTrace();
			}
		}

		return "Valeur introuvable"; // Gestion des erreurs

	}

	/**
	 * Assign collection.
	 *
	 * @param collectionInstanceDTOs the collection instance DT os
	 * @param collectionDTO the collection DTO
	 * @param action the action
	 * @param collectionStepDTOs the collection step DT os
	 * @return the acm collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public AcmCollectionDTO assignCollection(List<CollectionInstanceDTO> collectionInstanceDTOs,
			AcmCollectionDTO collectionDTO, String action,
			List<CollectionStepDTO> collectionStepDTOs) throws ResourcesNotFoundException {

		// find collection instance by pending action collection
		if (!ACMValidationUtils.isNullOrEmpty(collectionInstanceDTOs)) {
			// add get 0 collectionInstance
			// update
			int i = 0;
			Boolean findStep = Boolean.FALSE;
			if (action.equals("UPDATE")) {

				while (i < collectionInstanceDTOs.size() && !findStep) {
					if (collectionInstanceDTOs.get(i).getIdAcmCollectionStep()
							.equals(collectionDTO.getIdAcmCollectionStep().intValue())) {
						findStep = Boolean.TRUE;
					}
					i++;
				}
			}
			else if (action.equals("CREATE")
					&& !ACMValidationUtils.isNullOrEmpty(collectionInstanceDTOs.get(0))) {

				findStep = Boolean.TRUE;
			}

			int stepInstanceIndex = i;
			// instance exist first step
			if (findStep) {
				// save user action for collection instance
				// case update

				if (action.equals("UPDATE")) {
					collectionInstanceDTOs.get(stepInstanceIndex - 1)
							.setActionUser(userClient.find().getLogin());
					collectionInstanceService
							.save(collectionInstanceDTOs.get(stepInstanceIndex - 1));

					// case update
					// find step setting

					CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
					collectionStepDTO.setIdCollectionStep(collectionInstanceDTOs
							.get(stepInstanceIndex).getIdAcmCollectionStep().longValue());
					collectionStepDTO.setProductId(collectionDTO.getProductId());
					collectionStepDTO.setProcess(collectionDTO.getCollectionType());
					// init la list de setting
					collectionStepDTOs = new ArrayList<CollectionStepDTO>();
					collectionStepDTOs =
							settingWorkFlowService.findSettingCollection(collectionStepDTO);
				}
				// case add
				// get setting 0

				if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTOs)
						&& !ACMValidationUtils.isNullOrEmpty(collectionStepDTOs.get(0))) {
					CollectionStepDTO settingWorkFlowStepDTO = collectionStepDTOs.get(0);
					UserDTO userDTOResponsable = null;

					if (settingWorkFlowStepDTO.getStepType().equals("link")) {
						collectionDTO.setGroupOwner(null);
						collectionDTO.setGroupOwnerName(null);
						UserDTO userDTOPram = new UserDTO();
						// TODO CASE OF NEW COLLECTION ASSIGN TO THE OWNER
						switch (settingWorkFlowStepDTO.getPreviousStep()) {
							// Manager of loan owner
							case "-1":
								userDTOPram.setLogin(collectionDTO.getOwner());
								userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
								collectionDTO.setOwner(userDTOResponsable.getLogin());
								collectionDTO.setOwnerName(userDTOResponsable.getFullName());
								break;
							// Loan owner
							case "-2":

								break;
							// Manager of loan portfolio owner
							case "-3":
								// find the loan by loanIdExtern
								assignToPortfolioOwnerOrManager(collectionDTO, true);
								break;
							// Loan portfolio owner
							case "-4":
								assignToPortfolioOwnerOrManager(collectionDTO, false);

								break;
							default:
								// find step setting
								CollectionInstanceDTO linkedCollectionInstanceDTO =
										collectionInstanceDTOs.stream()
												.filter(instance -> Long
														.toString(instance.getOrderEtapeProcess())
														.equals(settingWorkFlowStepDTO
																.getPreviousStep()))
												.findFirst().orElse(null);
								if (!ACMValidationUtils
										.isNullOrEmpty(linkedCollectionInstanceDTO)) {
									userDTOPram
											.setLogin(linkedCollectionInstanceDTO.getActionUser());
									userDTOResponsable =
											userClient.findResponsibleOfUser(userDTOPram);
									collectionDTO.setOwner(userDTOResponsable.getLogin());
									collectionDTO.setOwnerName(userDTOResponsable.getFullName());
								}
								else {
									// TODO Exception
								}
								break;
						}
					}
					else if (settingWorkFlowStepDTO.getStepType().equals("group")) {
						// TODO check group
						collectionDTO.setOwner(null);
						collectionDTO.setOwnerName(null);
						collectionDTO.setGroupOwner(settingWorkFlowStepDTO.getGroupCode());
						collectionDTO.setGroupOwnerName(settingWorkFlowStepDTO.getUserGroup());
						setCollectionToGroupOfUsers(settingWorkFlowStepDTO.getGroupCode(),
								collectionDTO);
					}

				}
			}
			else {
				// TODO throw Exception
			}

		}
		return collectionDTO;

	}

	/**
	 * Assign to portfolio owner or manager.
	 *
	 * @author ymezrani
	 * @param collectionDTO the collection DTO
	 * @param findResponsible the find responsible
	 */
	private void assignToPortfolioOwnerOrManager(AcmCollectionDTO collectionDTO,
			Boolean findResponsible) {

		try {

			// find the loan by loanIdExtern
			LoanDTO loanDTO = creditClient.findByIdExtern(collectionDTO.getIdLoanExtern());
			if (!ACMValidationUtils.isNullOrEmpty(loanDTO)) {
				UserDTO userDTOPram = new UserDTO();
				userDTOPram.setAccountPortfolioId(loanDTO.getPortfolioId());
				UserDTO userDTO = null;
				if (findResponsible) {
					userDTO = userClient.findResponsibleOfUser(userDTOPram);
				}
				else {
					userDTO = userClient.find(userDTOPram).get(0);
				}
				if (!ACMValidationUtils.isNullOrEmpty(userDTO)) {
					collectionDTO.setOwner(userDTO.getLogin());
					collectionDTO.setOwnerName(userDTO.getFullName());
				}
			}
			else {
				UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
				collectionDTO.setOwner(userDTO.getLogin());
				collectionDTO.setOwnerName(userDTO.getLogin());
			}
		}
		catch (Exception e) {
			logger.error("Error while getting Loan relatedt to Collection from Credit Service = {}",
					e.getMessage());
		}
	}

	/**
	 * Sets the collection to group of users.
	 *
	 * @author idridi
	 * @param groupeCode the groupe code
	 * @param collectionDTO the collection DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void setCollectionToGroupOfUsers(String groupeCode, AcmCollectionDTO collectionDTO)
			throws ResourcesNotFoundException {

		// update collection data
		// find group by code group
		GroupeDTO groupeDTO = groupeService.findByCode(groupeCode);
		// get the list of users that have loanBranch in their accessBranch
		List<UserDTO> userDTOParam = userClient.getUsersWithLoanBranchInTheirAccessBranches(
				collectionDTO.getBranchId().intValue(), groupeDTO.getUserDTOs());

		// if the group has only one user( with loan branch IN his list of access branch) then
		// assign to this user
		if (userDTOParam.size() == 1) {
			collectionDTO.setOwner(userDTOParam.get(0).getLogin());
			collectionDTO.setOwnerName(userDTOParam.get(0).getSimpleName());
			collectionDTO.setGroupOwner(null);
			collectionDTO.setGroupOwnerName(null);
		}
		else {

			// set owner and owner name null
			collectionDTO.setOwner(null);
			collectionDTO.setOwnerName(null);
			// set group owner and group owner name
			collectionDTO.setGroupOwner(groupeDTO.getCode());
			collectionDTO.setGroupOwnerName(groupeDTO.getLibelle());
		}
	}

	/**
	 * Close task.
	 * 
	 * @author idridi
	 * @param collectionDTO the collection DTO
	 */
	public void closeTask(AcmCollectionDTO collectionDTO) {

		// check if object null
		Preconditions.checkNotNull(collectionDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// chekc if id object s null
		Preconditions.checkNotNull(collectionDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		crmClient.updateStatusTask(collectionDTO.getId());
		logger.info("Old tasks closed successfully, id collection {}", collectionDTO.getId());
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmCollectionService#assignCollection(com.acm.utils.dtos.AcmCollectionDTO)
	 */
	@Override
	public AcmCollectionDTO assignCollection(AcmCollectionDTO acmCollectionDTO)
			throws ResourcesNotFoundException, ParametrageException {

		Preconditions.checkNotNull(acmCollectionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(acmCollectionDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// load loan data by ID
		AcmCollection collection =
				collectionRepository.findById(acmCollectionDTO.getId()).orElse(null);
		// check if object is null
		if (collection == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmCollection.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmCollection.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + acmCollectionDTO.getId());
		}
		// throw error if the collection already assigned by another user
		if (!ACMValidationUtils.isNullOrEmpty(collection.getOwner())
				|| !ACMValidationUtils.isNullOrEmpty(collection.getOwnerName())) {
			throw new ParametrageException(
					new ExceptionResponseMessage(CommonErrorCode.COLLECTION_ALREADY_ASSIGNED,
							CommonExceptionsMessage.COLLECTION_ALREADY_ASSIGNED,
							new TechnicalException()),
					CommonExceptionsMessage.COLLECTION_ALREADY_ASSIGNED);
		}
		else {
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			// assign the collection to the connected user
			collection.setOwner(userDTO.getLogin());
			collection.setOwnerName(userDTO.getSimpleName());
			collection.setGroupOwner(null);
			collection.setGroupOwnerName(null);

			CommonFunctions.mapperToUpdate(collection, userClient, logger);
			AcmCollection updatedCollection = collectionRepository.save(collection);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmCollection.class.getSimpleName());
			// close collections
			crmClient.closeTasksForGrpOfUsers(updatedCollection.getId(),
					updatedCollection.getOwner());
			return mapper.map(updatedCollection, AcmCollectionDTO.class);
		}
	}

	/**
	 * Notif collection.
	 *
	 * @param collection the collection
	 */
	public void NotifCollection(AcmCollectionDTO collection) {

		CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
		List<CollectionStepDTO> listCollectionStep = new ArrayList<>();
		UserDTO userDto = new UserDTO();
		// Check if available Date is old than sys date
		if (collection.getAvailableDate().before(new Date())) {
			// Calculate number of days between today and the available date of collection
			Long days = DateUtil.calculateDaysBetweenTwoDates(collection.getAvailableDate(),
					new Date());
			collectionStepDTO.setIdCollectionStep(collection.getIdAcmCollectionStep());
			collectionStepDTO.setProductId(collection.getProductId());
			listCollectionStep = settingWorkFlowService.findSettingCollection(collectionStepDTO);
			if (!ACMValidationUtils.isNullOrEmpty(listCollectionStep)) {
				// Check if step is the current step
				// check if one of user group or user exist
				// and check if reminder equal to days
				if (listCollectionStep.get(0).getReminder() == days.intValue()
						&& listCollectionStep.get(0).getPreviousStep() != null
						|| listCollectionStep.get(0).getUserGroup() != null) {
					// send notification to owner
					NotificationsDTO notificationsDTO = new NotificationsDTO();
					notificationsDTO =
							creditClient.create(new NotificationsDTO(collection.getOwner(),
									listCollectionStep.get(0).getStepName(),
									CommonConstants.REMINDER_COLLECTION_CATEGORY,
									NotificationType.INFO.name(), Boolean.TRUE,
									NotificationStatut.NEW.name(), collection.getOwnerName(),
									collection.getCustomerName(),
									DateUtil.addDays(collection.getAvailableDate(),
											listCollectionStep.get(0).getStartDate()),
									collection.getId()));

					logger.info("friendly reminder Notification [{}] has been inserted.",
							notificationsDTO);

				}
				if (listCollectionStep.get(0).getReminderSup() == days.intValue()
						&& listCollectionStep.get(0).getUserGroup() != null
						|| listCollectionStep.get(0).getPreviousStep() != null) {
					// Get responsible of User
					userDto.setLogin(collection.getOwner());
					UserDTO responsibleUserDTO = userClient.findResponsibleOfUser(userDto);
					// send notification to responsible of the owner
					NotificationsDTO notificationsDTO = new NotificationsDTO();
					notificationsDTO =
							creditClient.create(new NotificationsDTO(responsibleUserDTO.getLogin(),
									listCollectionStep.get(0).getStepName(),
									CommonConstants.REMINDER_SUP_COLLECTION_CATEGORY,
									NotificationType.INFO.name(), Boolean.TRUE,
									NotificationStatut.NEW.name(), collection.getOwnerName(),
									collection.getCustomerName(),
									DateUtil.addDays(collection.getAvailableDate(),
											listCollectionStep.get(0).getStartDate()),
									collection.getId()));
					logger.info("friendly reminder sup Notification [{}] has been inserted.",
							notificationsDTO);

				}
			}
		}
	}

	/**
	 * Sets the charge fee.
	 *
	 * @param ChargeFeesDTOs the charge fees DT os
	 * @param responseChargeFeeDTO the response charge fee DTO
	 * @return the response charge fee DTO
	 */
	ResponseChargeFeeDTO setChargeFee(List<ChargeFeesDTO> ChargeFeesDTOs,
			ResponseChargeFeeDTO responseChargeFeeDTO) {

		for (ChargeFeesDTO item : ChargeFeesDTOs) {
			ChargeFeeDTO chargeFeeDTO = new ChargeFeeDTO();
			chargeFeeDTO.setManuallyAdded(true);
			chargeFeeDTO.setCanEditAmount(true);
			chargeFeeDTO.setPaid(false);
			chargeFeeDTO.setCharged(true);
			chargeFeeDTO.setAllowAmountChange(true);
			chargeFeeDTO.setVat(false);
			chargeFeeDTO.setCuFeeID(item.getCufeeId());
			chargeFeeDTO.setAmount(item.getAmount());
			chargeFeeDTO.setOpenBalance(item.getAmount());
			(responseChargeFeeDTO.getFees()).add(chargeFeeDTO);
		}
		return responseChargeFeeDTO;
	}

	/**
	 * Fees to charge.
	 *
	 * @param loanDTO the loan DTO
	 * @param actualCollectionStepDTO the actual collection step DTO
	 * @param currentCollectionInstanceDTO the current collection instance DTO
	 * @param feesToCharge the fees to charge
	 * @return the list
	 */
	List<ChargeFeesDTO> addAutomaticFees(LoanDTO loanDTO, CollectionStepDTO actualCollectionStepDTO,
			CollectionInstanceDTO currentCollectionInstanceDTO, List<ChargeFeesDTO> feesToCharge) {

		for (SettingChargeFeeDTO item : actualCollectionStepDTO.getListChargeFees()) {
			if (!item.getValue().equals(CommonConstants.MANUAL_ENTRY)) {
				ChargeFeesDTO chargefee = new ChargeFeesDTO();
				chargefee.setCode(item.getCode());
				chargefee.setLabel(item.getLabel());
				chargefee.setValue(item.getValue());
				chargefee.setIdCollectionInstance(currentCollectionInstanceDTO.getId());
				chargefee.setCufeeId(item.getCufeeId());
				chargefee.setSettingFee(item.getId());
				if (CommonConstants.FIXED_AMOUNT.equals(item.getValue())) {
					if (item.getAmount() != null && !item.getAmount().equals(BigDecimal.ZERO)) {
						chargefee.setAmount(item.getAmount());
					}
				}
				if (CommonConstants.PERSONNAL_CONTRIBUTION.equals(item.getValue())) {
					BigDecimal chargeAmount = new BigDecimal(loanDTO.getPersonalContribution())
							.multiply(BigDecimal.valueOf(item.getPercentage()))
							.divide(BigDecimal.valueOf(100));
					chargefee.setAmount(chargeAmount);
				}
				if (CommonConstants.LOAN_AMOUNT.equals(item.getValue())) {
					BigDecimal chargeAmount = loanDTO.getApprovelAmount()
							.multiply(BigDecimal.valueOf(item.getPercentage()))
							.divide(BigDecimal.valueOf(100));
					chargefee.setAmount(chargeAmount);
				}
				feesToCharge.add(chargefee);
			}
		}
		return feesToCharge;
	}

	/**
	 * Adds the loan note.
	 *
	 * @param acmCollectionDTO the acm collection DTO
	 * @param feesToCharge the fees to charge
	 */
	void addCollectionNote(AcmCollectionDTO acmCollectionDTO, List<ChargeFeesDTO> feesToCharge) {

		for (ChargeFeesDTO item : feesToCharge) {
			CollectionNoteDTO feeNoteDTO = new CollectionNoteDTO();
			feeNoteDTO.setCollectionId(acmCollectionDTO.getId());
			feeNoteDTO.setAction(acmCollectionDTO.getStatutLibelleDone());
			feeNoteDTO.setComment(
					"CHARGING FEES: " + item.getCode() + "= " + item.getAmount().toString());

			if (CommonConstants.MANUAL_ENTRY.equals(item.getValue())) {
				collectionNoteService.save(feeNoteDTO);
			}
			else {
				collectionNoteService.save(feeNoteDTO, "SYSTEM");
			}
		}
	}
}
