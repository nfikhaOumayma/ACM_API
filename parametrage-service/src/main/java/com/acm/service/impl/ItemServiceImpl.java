/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.dozer.MappingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.CrmClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantItemStatuts;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.repository.ItemInstanceRepository;
import com.acm.repository.ItemRepository;
import com.acm.repository.ItemRiskSettingRepository;
import com.acm.repository.SettingWorkFlowRepository;
import com.acm.repository.StepRiskSettingRepository;
import com.acm.service.ItemNoteService;
import com.acm.service.ItemService;
import com.acm.service.SettingWorkFlowService;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.ItemInstanceDTO;
import com.acm.utils.dtos.ItemNoteDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.dtos.pagination.ItemPaginationDTO;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;
import com.acm.utils.models.AcmDocuments;
import com.acm.utils.models.Item;
import com.acm.utils.models.ItemInstance;
import com.acm.utils.models.ItemRiskSetting;
import com.acm.utils.models.QItem;
import com.acm.utils.models.StepRiskSetting;
import com.acm.utils.models.WorkFlowStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class ItemServiceImpl.
 */
@Service
public class ItemServiceImpl implements ItemService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ItemServiceImpl.class);

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The item repository. */
	@Autowired
	private ItemRepository itemRepository;

	/** The setting work flow service. */
	@Autowired
	private SettingWorkFlowService settingWorkFlowService;

	/** The item instance repository. */
	@Autowired
	private ItemInstanceRepository itemInstanceRepository;

	/** The step risk setting repository. */
	@Autowired
	private StepRiskSettingRepository stepRiskSettingRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The setting work flow repository. */
	@Autowired
	private SettingWorkFlowRepository settingWorkFlowRepository;

	/** The item risk setting repository. */
	@Autowired
	private ItemRiskSettingRepository itemRiskSettingRepository;

	/** The item note service. */
	@Autowired
	private ItemNoteService itemNoteService;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The crm client. */
	@Autowired
	private CrmClient crmClient;

	/**
	 * The item. * /* (non-Javadoc)
	 * 
	 * @see com.acm.service.ItemService#save(com.acm.utils.dtos.ItemDTO)
	 */
	private Item item;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemService#save(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public ItemDTO save(ItemDTO itemDTO) throws WorkFlowSettingException, MappingException {

		// transform the Branches from string to List
		List<String> listBranchIds = Arrays.asList(itemDTO.getBranches().split(","));
		// the loop for assign the access branch to item
		for (String branche : listBranchIds) {
			// item = new Item();
			itemDTO.setBranches(branche);
			// set id to null for a new insert
			itemDTO.setId(null);
			List<WorkFlowStepDTO> lstWorkFlow = null;

			WorkFlowStepDTO workFlowStepDTO = new WorkFlowStepDTO();
			workFlowStepDTO.setProcess(CommonConstants.GENERIC_WORKFLOW_WORKFLOW_PROCESS);
			workFlowStepDTO.setEnabled(true);
			workFlowStepDTO.setProductId(itemDTO.getGenericWorkFlowObject().getId());
			try {
				lstWorkFlow = settingWorkFlowService.findSteps(workFlowStepDTO);
				// get the first step
				WorkFlowStepDTO actualStep =
						lstWorkFlow.stream().filter(element -> element.getOrder() == 0)
								.collect(Collectors.toList()).get(0);
				itemDTO.setEnabled(true);
				itemDTO.setStatus(Integer.parseInt(actualStep.getCodeStatutLoan().toString()));
				item = mapper.map(itemDTO, Item.class);
				CommonFunctions.mapperToSave(item, userClient, logger);
				// first save item information
				item = itemRepository.save(item);
				itemDTO = mapper.map(item, ItemDTO.class);
				// get the active instance
				List<ItemInstance> disableItemInstances =
						itemInstanceRepository.findByEnabled(true);
				// disable the active instance
				disableItemInstances.forEach(element -> element.setEnabled(false));
				itemInstanceRepository.saveAll(disableItemInstances);
				WorkFlowStep workFlowStep = new WorkFlowStep();
				List<ItemInstanceDTO> lstInstance = new ArrayList<>();
				// create the new instance
				lstWorkFlow.forEach(element -> {
					ItemInstance itemInstance = new ItemInstance();
					itemInstance.setItem(item);
					itemInstance.setIdWorkFlowStep(element.getIdWorkFlowStep());
					itemInstance.setLibelle(element.getStepName());
					itemInstance.setOrderEtapeProcess(element.getOrder());
					itemInstance.setCodeStatutItem(element.getCodeStatutLoan());
					itemInstance.setIhmRoot(element.getScreen());
					workFlowStep.setIdWorkFlowStep(element.getIdWorkFlowStep());
					CommonFunctions.mapperToSave(itemInstance, userClient, logger);
					itemInstance.setEnabled(true);
					ItemInstance itemInstanceSaving = itemInstanceRepository.save(itemInstance);
					lstInstance.add(mapper.map(itemInstanceSaving, ItemInstanceDTO.class));

				});
				// get the list of risk by active process workFlow
				List<StepRiskSetting> lstStepRisks = stepRiskSettingRepository
						.findByWorkFlowStepProcessAndWorkFlowStepEnabledAndWorkFlowStepProductId(
								CommonConstants.GENERIC_WORKFLOW_WORKFLOW_PROCESS, true,
								itemDTO.getGenericWorkFlowObject().getId());

				List<StepRiskSetting> uniqueList = lstStepRisks.stream()
						.filter(CommonFunctions.distinctByKey(StepRiskSetting::getSettingTypeRisk))
						.collect(Collectors.toList());
				// save risk by item
				uniqueList.forEach(stepRisk -> {

					ItemRiskSetting itemRiskSetting = new ItemRiskSetting();
					itemRiskSetting.setItem(item);
					itemRiskSetting.setSettingTypeRisk(stepRisk.getSettingTypeRisk());
					itemRiskSettingRepository.save(itemRiskSetting);

				});

				ItemInstanceDTO actualStepInstance =
						lstInstance.stream().filter(element -> element.getOrderEtapeProcess() == 0)
								.collect(Collectors.toList()).get(0);

				// initialize the item by the workFlow step and actualStepInstance
				item.setActualStep(actualStep.getIdWorkFlowStep());
				item.setActualStepInstance(actualStepInstance.getId());

				itemDTO = assginItemToOwner(actualStepInstance, actualStep,
						mapper.map(item, ItemDTO.class), lstInstance);

				lstInstance.forEach(instance -> {
					if (instance.getIdWorkFlowStep().equals(actualStep.getIdWorkFlowStep())) {
						instance.setActionUser(CommonFunctions.getConnectedUser(logger).getLogin());
					}
				});
				if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getGroupOwner())) {
					itemDTO.setStatus(CommonFunctions
							.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_UNASSIGNED).getKey()); // unassigned
				}
				item = mapper.map(itemDTO, Item.class);
				item = itemRepository.save(item);
				logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
						AcmDocuments.class.getSimpleName());
			}
			catch (WorkFlowSettingException e) {
				logger.error("error in  save item ");
				logger.error(e.getMessage());
			}

		}
		return itemDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ItemService#findPagination(com.acm.utils.dtos.pagination.ItemPaginationDTO)
	 */
	@Override
	public ItemPaginationDTO findPagination(ItemPaginationDTO itemPaginationDTO) {

		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		Preconditions.checkNotNull(itemPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init qExceptionRequest
		QItem qItem = QItem.item;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(itemPaginationDTO.getParams(), qItem);
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		// default sort by applyDate : DESC
		pageable = PageRequest.of(itemPaginationDTO.getPageNumber(),
				itemPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		// QueryDSL using springDATA

		Page<Item> pageResult = itemRepository.findAll(predicate, pageable);
		if (pageResult.hasContent()) {
			List<Item> items = pageResult.getContent();
			logger.info(
					"find by pagination method : {} : Loan was founded (PageNumber = {} / PageSize = {} )",
					items.size(), itemPaginationDTO.getPageNumber(),
					itemPaginationDTO.getPageSize());
			List<ItemDTO> ItemDTOs = new ArrayList<>();
			items.forEach(itemReq -> {
				ItemDTO itemDTORequestDTOResultat = mapper.map(itemReq, ItemDTO.class);
				ItemDTOs.add(itemDTORequestDTOResultat);
			});
			// setting data
			itemPaginationDTO.setResult(ItemDTOs);
			// setting totals pages
			itemPaginationDTO.setTotalElements(pageResult.getTotalElements());
			// setting totals elements
			itemPaginationDTO.setTotalPages(pageResult.getTotalPages());
		}
		return itemPaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @param itemDTO the item DTO
	 * @param qItem the q item
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(ItemDTO itemDTO, QItem qItem) {

		// init Predicate
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qItem.enabled.eq(Boolean.TRUE));

		// find by statut

		if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getStatus()) && itemDTO.getStatus() != 0) {

			if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getUnassignedItemStatus())
					&& !ACMValidationUtils.isNullOrEmpty(itemDTO.getStatus())) {
				BooleanBuilder subPredicate = new BooleanBuilder();
				subPredicate.and(qItem.status.eq(itemDTO.getStatus()));

				subPredicate.or(qItem.status.eq(itemDTO.getUnassignedItemStatus()));
				predicate.and(subPredicate);
			}
			else if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getStatus())) {
				predicate.and(qItem.status.eq(itemDTO.getStatus()));
			}

			if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getDateInsertion())) {
				predicate.and(qItem.dateInsertion.eq(itemDTO.getDateInsertion()));
			}

			if (!ACMValidationUtils.isNullOrEmpty(itemDTO.getBranches())) {
				predicate.and(qItem.branches.like("%" + itemDTO.getBranches() + "%"));
			}

			List<String> wheresOwners = new ArrayList<>();
			List<UserDTO> userDTOs = userClient.findUsers();
			userDTOs.forEach(user -> {
				if (!user.getTypeUser().equals(UserHierarchicalType.SUPERVISOR.name())) {
					wheresOwners.add(user.getLogin());
				}
			});
			BooleanBuilder subOwnerPredicate = new BooleanBuilder();
			// setting predicate to find by Id
			subOwnerPredicate.and(qItem.owner.isNull());
			subOwnerPredicate.and(qItem.branches.eq(userDTO.getBranchID().toString()));
			if (userDTO.getCategory() != null
					&& userDTO.getCategory().equals(UserCategory.OPERATION.name())) {
				subOwnerPredicate.or(qItem.owner.in(new ArrayList<>(new HashSet<>(wheresOwners))));
			}
			predicate.and(subOwnerPredicate);
			// find loan by Access Branches for connected user
			if (userDTO.getCategory() != null
					&& userDTO.getCategory().equals(UserCategory.MANAGMENT.name())
					&& !ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				List<String> listBranchIds = new ArrayList<>(arrayBranchIds.length);
				for (int i : arrayBranchIds) {
					listBranchIds.add(String.valueOf(i));
				}
				// setting predicate to find by given branch Id
				BooleanBuilder subPredicate = new BooleanBuilder();
				subPredicate.and(qItem.branches.in(listBranchIds));
				// find by given status

				subPredicate.and(qItem.status.eq(itemDTO.getStatus()));
				predicate.or(subPredicate);

			}
			else {
				// load loan by branch ID for SUPERVISOR
				boolean isManagerBranch = userDTOs.stream().anyMatch(user -> user.getTypeUser()
						.equals(UserHierarchicalType.COLLABORATORS.name()));
				if (isManagerBranch) {
					// setting predicate to find by given branch Id
					BooleanBuilder subPredicate = new BooleanBuilder();
					subPredicate.and(qItem.branches.eq(userDTO.getBranchID().toString()));
					// find by given status
					subPredicate.and(qItem.status.eq(itemDTO.getStatus()));
					predicate.or(subPredicate);
				}

			}

		}
		else if (ACMValidationUtils.isNullOrEmpty(itemDTO.getStatus())
				|| (!ACMValidationUtils.isNullOrEmpty(itemDTO.getStatus())
						&& itemDTO.getStatus() == 0)) {
			predicate.and(qItem.owner.eq(userDTO.getLogin()));

			// for the TAB "My Tasks" Excluded statutWorkflow :
			// closed / rejected/review

			predicate.and(qItem.status.ne(CommonFunctions
					.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_CLOSED).getKey())); // closed
			predicate.and(qItem.status.ne(CommonFunctions
					.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REJECTED).getKey())); // rejected
			predicate.and(qItem.status.ne(CommonFunctions
					.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REVIEW).getKey())); // review
			predicate.and(qItem.status.ne(CommonFunctions
					.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_UNASSIGNED).getKey())); // unassined

		}

		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemService#nextStep(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public ItemDTO nextStep(ItemDTO itemDtoForAssignUser)
			throws ConditionalApproveException, WorkFlowSettingException, MappingException {

		final ItemDTO itemDTO = itemDtoForAssignUser;
		List<ItemInstanceDTO> itemInstanceDTOs = itemDTO.getItemInstanceDTOs();
		List<Long> lstWorkFlowIds = new ArrayList<>();
		// fill lstWorkFlowIds with the steps item
		itemInstanceDTOs.stream()
				.forEach(element -> lstWorkFlowIds.add(element.getIdWorkFlowStep()));
		List<WorkFlowStep> workFlowSteps = settingWorkFlowRepository.findAllById(lstWorkFlowIds);
		// actual step for item
		WorkFlowStep actualStep = workFlowSteps.stream()
				.filter(element -> element.getIdWorkFlowStep().equals(itemDTO.getActualStep()))
				.collect(Collectors.toList()).get(0);
		// get actual setep instance
		ItemInstanceDTO actualStepInstance = itemInstanceDTOs.stream()
				.filter(element -> itemDTO.getActualStepInstance().equals(element.getId()))
				.collect(Collectors.toList()).get(0);
		// isNextStepExist return true if item don't have nextStep else return false
		Boolean hasNextStep = isNextStepExist(workFlowSteps, actualStep);
		WorkFlowStep nextStep = null;

		// if item doesn't have value in reviewFromStep
		// then the item take the normal process of WF
		// else the item will move to the step from where he come
		if (ACMValidationUtils.isNullOrEmpty(itemDTO.getReviewFromStep())) {
			// if item don't have a next step then change status to closed
			if (!hasNextStep) {
				// get the nextStepInstance
				ItemInstanceDTO nextStepInstance = itemInstanceDTOs.stream()
						.filter(element -> element.getOrderEtapeProcess()
								.equals(actualStepInstance.getOrderEtapeProcess() + 1))
						.collect(Collectors.toList()).get(0);
				// get the nextStep
				nextStep = workFlowSteps.stream()
						.filter(element -> element.getOrder().equals(actualStep.getOrder() + 1))
						.collect(Collectors.toList()).get(0);
				itemDTO.setStatus(Integer.parseInt(nextStep.getCodeStatutLoan().toString()));
				itemDTO.setActualStep(nextStep.getIdWorkFlowStep());
				itemDTO.setActualStepInstance(nextStepInstance.getId());
				itemDtoForAssignUser = assginItemToOwner(nextStepInstance,
						mapper.map(nextStep, WorkFlowStepDTO.class), itemDTO, itemInstanceDTOs);
				itemDTO.setOwner(itemDtoForAssignUser.getOwner());
				itemDTO.setOwnerName(itemDtoForAssignUser.getOwnerName());
				itemDTO.setOwnerEmail(itemDtoForAssignUser.getOwnerEmail());
				nextStepInstance.setActionUser(CommonFunctions.getConnectedUser(logger).getLogin());
				ItemInstance ItemInstanceTosave = (ItemInstance) CommonFunctions.mapperToSave(
						mapper.map(nextStepInstance, ItemInstance.class), userClient, logger);
				// set user action and save instance
				itemInstanceRepository.save(ItemInstanceTosave);

			}
			// verify conditional approve in actual step in workFlow
			if (!ACMValidationUtils.isNullOrEmpty(actualStep)
					&& Boolean.TRUE.equals(actualStep.getApprovalConditions())
					&& creditClient.countConditionnalApproveByItem(itemDTO.getId()) > 0) {

				throw new ConditionalApproveException(CommonErrorCode.APPROVAL_EXCEPTION,
						"Please check the approval conditions");
			}

			// if workflow don't have a next step change status to closed
			if (hasNextStep) {
				itemDTO.setStatus(CommonFunctions
						.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_CLOSED).getKey()); // closed

			}

			if (!ACMValidationUtils.isNullOrEmpty(nextStep)
					&& Boolean.TRUE.equals(nextStep.getGenerationTask())
					&& (itemDTO.getOwner() != null || itemDTO.getGroupOwner() != null)) {
				// generate task For users
				try {
					generateTasks(itemDTO);
				}
				catch (Exception e) {
					logger.error("error in generate task for Item ");
				}
			}
		}
		else {
			// review
			itemDTO.setActualStep(itemDTO.getReviewFromStep());
			itemDTO.setActualStepInstance(itemInstanceDTOs.stream()
					.filter(element -> element.getIdWorkFlowStep()
							.equals(itemDTO.getReviewFromStep()))
					.collect(Collectors.toList()).get(0).getId());
			itemDTO.setReviewFromStep(null);
		}

		item = mapper.map(itemDTO, Item.class);
		CommonFunctions.mapperToSave(item, userClient, logger);
		itemDTO.setStatusLabel(CommonFunctions
				.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_APPROVED).getValue());
		saveNote(itemDTO);
		return mapper.map(itemRepository.save(item), ItemDTO.class);
	}

	/**
	 * Generate tasks.
	 *
	 * @param itemDTO the item DTO
	 */
	public void generateTasks(ItemDTO itemDTO) {

		CalendarEventDTO calenderEventDTO = new CalendarEventDTO();
		calenderEventDTO.setIdItem(itemDTO.getId());

		Calendar calendar = Calendar.getInstance();

		// fill task data
		calenderEventDTO.setLibelleEvent(CommonConstants.ITEM_CATEGORY);
		calendar.set(Calendar.HOUR_OF_DAY, 8); // 24-hour format
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		calenderEventDTO.setDateDebut(calendar.getTime());
		calendar.set(Calendar.HOUR_OF_DAY, 18);
		calenderEventDTO.setDateFin(calendar.getTime());
		calenderEventDTO.setTypeEvent(ACMConstantWorkflowStatuts.NEXT_ACTION_TASK);
		calenderEventDTO.setCategory(CommonConstants.ITEM_CATEGORY);
		calenderEventDTO.setDescription(CommonConstants.ITEM_CATEGORY);

		if (itemDTO.getOwner() != null) {

			calenderEventDTO.setUsername(itemDTO.getOwner());
			calenderEventDTO.setUserEmail(itemDTO.getOwnerEmail());

		}

		crmClient.createLoanTaskAndCloseOldTask(calenderEventDTO, Boolean.TRUE, Boolean.FALSE,
				Boolean.TRUE);

	}

	/**
	 * Checks if is next step exist.
	 *
	 * @param workFlowSteps the work flow steps
	 * @param actualStep the actual step
	 * @return the boolean
	 */
	private Boolean isNextStepExist(List<WorkFlowStep> workFlowSteps, WorkFlowStep actualStep) {

		List<WorkFlowStep> nextSteps = workFlowSteps.stream()
				.filter(element -> element.getOrder().equals(actualStep.getOrder() + 1))
				.collect(Collectors.toList());

		return ACMValidationUtils.isNullOrEmpty(nextSteps);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemService#reject(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public ItemDTO reject(ItemDTO itemDTO) {

		itemDTO.setStatusLabel(CommonFunctions
				.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REJECTED).getValue());
		saveNote(itemDTO);
		itemDTO.setStatus(
				CommonFunctions.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REJECTED).getKey());
		itemRepository.save(mapper.map(itemDTO, Item.class));
		return itemDTO;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemService#review(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public ItemDTO review(ItemDTO itemDTO) {

		itemDTO.setStatusLabel(
				CommonFunctions.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REVIEW).getValue());
		itemDTO.setStatus(
				CommonFunctions.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_REVIEW).getKey());
		itemRepository.save(mapper.map(itemDTO, Item.class));
		saveNote(itemDTO);
		return itemDTO;
	}

	/**
	 * Save note.
	 *
	 * @param itemDTO the item DTO
	 */
	public void saveNote(ItemDTO itemDTO) {

		ItemNoteDTO itemNoteDTO = new ItemNoteDTO();
		if (itemDTO.getStatusLabel().equals(CommonFunctions
				.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_APPROVED).getValue())) {
			itemNoteDTO.setComment(CommonFunctions
					.mappingStatus(ACMConstantItemStatuts.STATUS_TAB_APPROVED).getValue());
		}
		else {
			itemNoteDTO.setComment(itemDTO.getReasonLabel());
		}
		itemNoteDTO.setItemId(itemDTO.getId());
		itemNoteDTO.setAction(itemDTO.getStatusLabel());
		itemNoteService.save(itemNoteDTO);
	}

	/**
	 * Assgin item to owner.
	 *
	 * @param nextItemInstanceStep the next item instance step
	 * @param nextWorkFlowStepDTO the next work flow step DTO
	 * @param itemDTO the item DTO
	 * @param itemInstanceDTOs the item instance DT os
	 * @return the item DTO
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	public ItemDTO assginItemToOwner(ItemInstanceDTO nextItemInstanceStep,
			WorkFlowStepDTO nextWorkFlowStepDTO, ItemDTO itemDTO,
			List<ItemInstanceDTO> itemInstanceDTOs) throws WorkFlowSettingException {

		UserDTO userDTOResponsable = null;
		if (!ACMValidationUtils.isNullOrEmpty(nextItemInstanceStep.getActionUser())) {
			itemDTO.setOwner(nextItemInstanceStep.getActionUser());

			List<UserDTO> userDTOs =
					userClient.find(new UserDTO(nextItemInstanceStep.getActionUser()));

			if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
				itemDTO.setOwnerName(userDTOs.get(0).getFullName());
				itemDTO.setOwnerEmail(userDTOs.get(0).getEmail());
			}
		}
		else if (nextWorkFlowStepDTO.getStepType().equals("link")) {
			itemDTO.setGroupOwner(null);
			itemDTO.setGroupOwnerName(null);
			UserDTO userDTOPram = new UserDTO();
			switch (nextWorkFlowStepDTO.getPreviousStep()) {
				// Manager of loan owner
				case "-1":
					userDTOPram.setLogin(itemDTO.getOwner());
					userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
					itemDTO.setOwner(userDTOResponsable.getLogin());
					itemDTO.setOwnerName(userDTOResponsable.getFullName());
					itemDTO.setOwnerEmail(userDTOResponsable.getEmail());
					break;
				// Loan owner
				case "-2":
					// itemDTO.setIhmRoot(nextItemInstanceStep.getIhmRoot());
					break;
				// Manager of loan portfolio owner
				case "-3":
					userDTOPram.setAccountPortfolioId(itemDTO.getPortfolioId());
					userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
					itemDTO.setOwner(userDTOResponsable.getLogin());
					itemDTO.setOwnerName(userDTOResponsable.getFullName());
					itemDTO.setOwnerEmail(userDTOResponsable.getEmail());
					break;
				// Loan portfolio owner
				case "-4":
					userDTOPram.setAccountPortfolioId(itemDTO.getPortfolioId());
					UserDTO userDTO = userClient.find(userDTOPram).get(0);
					itemDTO.setOwner(userDTO.getLogin());
					itemDTO.setOwnerName(userDTO.getFullName());
					itemDTO.setOwnerEmail(userDTO.getEmail());
					break;
				default:
					// find step setting
					String previousStepCode = nextWorkFlowStepDTO.getPreviousStep();
					ItemInstanceDTO linkedLoanInstanceDTO =
							itemInstanceDTOs.stream()
									.filter(instance -> Long
											.toString(instance.getOrderEtapeProcess())
											.equals(previousStepCode))
									.findFirst().orElse(null);

					if (!ACMValidationUtils.isNullOrEmpty(linkedLoanInstanceDTO)) {
						userDTOPram.setLogin(linkedLoanInstanceDTO.getActionUser());
						userDTOResponsable = userClient.findResponsibleOfUser(userDTOPram);
						itemDTO.setOwner(userDTOResponsable.getLogin());
						itemDTO.setOwnerName(userDTOResponsable.getFullName());
						itemDTO.setOwnerEmail(userDTOResponsable.getEmail());
					}
					else {
						throw new WorkFlowSettingException(
								new ExceptionResponseMessage(
										CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
										CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND,
										new TechnicalException()),
								CommonExceptionsMessage.WORKFLOW_INSTANCE_NOT_FOUND);
					}

					break;
			}

		}
		else if (nextWorkFlowStepDTO.getStepType().equals("group")) {
			itemDTO = assignLoanToGroupOfUsers(nextWorkFlowStepDTO.getGroupCode(), itemDTO);
		}

		return itemDTO;
	}

	/**
	 * Assign loan to group of users.
	 *
	 * @param groupeCode the groupe code
	 * @param itemDTO the item DTO
	 * @return the item DTO
	 */
	private ItemDTO assignLoanToGroupOfUsers(String groupeCode, ItemDTO itemDTO) {

		// get the list of users that have loanBranch in their accessBranch
		List<UserDTO> userDTOParam = userClient.findByGroupeCodeAndBranchIDAndAccessBranches(
				groupeCode, Integer.parseInt(itemDTO.getBranches()));

		// if the group has only one user( with loan branch IN his list of access branch) then
		// assign to this user
		if (userDTOParam.size() == 1) {
			itemDTO.setOwner(userDTOParam.get(0).getLogin());
			itemDTO.setOwnerName(userDTOParam.get(0).getSimpleName());
			itemDTO.setGroupOwner(null);
			itemDTO.setGroupOwnerName(null);

		}
		else {
			// set owner and owner name null
			itemDTO.setOwner(null);
			itemDTO.setOwnerName(null);
			// set owner group and owner group name
			itemDTO.setGroupOwner(userDTOParam.get(0).getGroupes().iterator().next().getCode());
			itemDTO.setGroupOwnerName(
					userDTOParam.get(0).getGroupes().iterator().next().getLibelle());
		}
		return itemDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemService#assignItem(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public ItemDTO assignItem(ItemDTO itemDTO) {

		// assign item from group to user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);

		itemDTO.setOwner(userDTO.getLogin());
		itemDTO.setOwnerName(userDTO.getFullName());
		itemDTO.setGroupOwner(null);
		itemDTO.setGroupOwnerName(null);
		Item itemToUpdate = (Item) CommonFunctions.mapperToUpdate(mapper.map(itemDTO, Item.class),
				userClient, logger);
		settingWorkFlowRepository.findById(itemDTO.getActualStep()).get().getCodeStatutLoan();
		itemToUpdate.setStatus(Integer.parseInt(settingWorkFlowRepository
				.findById(itemDTO.getActualStep()).get().getCodeStatutLoan().toString()));
		return mapper.map(itemRepository.save(itemToUpdate), ItemDTO.class);
	}

}
