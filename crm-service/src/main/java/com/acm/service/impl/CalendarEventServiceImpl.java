/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CalendarEventRepository;
import com.acm.service.CalendarEventService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.EventPaginationDTO;
import com.acm.utils.models.CalendarEvent;
import com.acm.utils.models.QCalendarEvent;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.OrderSpecifier;

// TODO: Auto-generated Javadoc
/**
 * {@link CalendarEventServiceImpl } class.
 *
 * @author MoezMhiri
 * @since 0.5.0
 */
@Service
public class CalendarEventServiceImpl implements CalendarEventService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CalendarEventServiceImpl.class);

	/** The calendar event repository. */
	@Autowired
	private CalendarEventRepository calendarEventRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The reporting client. */
	@Autowired
	private ReportingClient reportingClient;
	/** The reporting client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventServiceImp#find(java.lang.Long)
	 */
	@Override
	public CalendarEventDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find calendarEvent by ID : {}", id);
		CalendarEvent calendarEvent = calendarEventRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(calendarEvent)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, CalendarEvent.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ CalendarEvent.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(calendarEvent, CalendarEventDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventServiceImp#find(com.acm.utils.dtos.CalendarEventDTO)
	 */
	@Override
	public List<CalendarEventDTO> find(CalendarEventDTO calendarEventDto) {

		Preconditions.checkNotNull(calendarEventDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QCalendarEvent
		QCalendarEvent qCalendarEvent = QCalendarEvent.calendarEvent;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		if (Boolean.TRUE.equals(calendarEventDto.getAllTeamsTasks())) {
			// find calendarEvent only for connected user
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			List<UserDTO> teamUserDTOs = userClient.find(new UserDTO(null, null, userDTO.getLogin(),
					null, null, null, null, null, null));
			List<String> teamLogins = new ArrayList<>();
			teamUserDTOs.forEach(d -> teamLogins.add(d.getLogin()));
			// find calendarEvent of connected user and of all his user's team
			predicate.and(qCalendarEvent.username.in(teamLogins));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getUsername())) {
			predicate.and(qCalendarEvent.username.eq(calendarEventDto.getUsername()));
		}
		// find calendarEvent by typeEvent
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getTypeEvent())) {
			predicate.and(qCalendarEvent.typeEvent.eq(calendarEventDto.getTypeEvent()));
		}
		// find calendarEvent by extern item id for generic workFlow
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getIdItem())) {
			predicate.and(qCalendarEvent.idItem.eq(calendarEventDto.getIdItem()));
		}
		// find calendarEvent by extern loan id
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getIdLoanExtern())) {
			predicate.and(qCalendarEvent.idLoanExtern.eq(calendarEventDto.getIdLoanExtern()));
		}
		// find calendarEvent by collection id
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getIdCollection())) {
			predicate.and(qCalendarEvent.idCollection.eq(calendarEventDto.getIdCollection()));
		}
		// find calendarEvent by claim id
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getIdClaim())) {
			predicate.and(qCalendarEvent.idClaim.eq(calendarEventDto.getIdClaim()));
		}
		// find calendarEvent by category
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getCategory())) {
			predicate.and(qCalendarEvent.category.eq(calendarEventDto.getCategory()));
		}
		// find calendarEvent by statut
		if (!ACMValidationUtils.isNullOrEmpty(calendarEventDto.getStatut())) {
			predicate.and(qCalendarEvent.statut.eq(calendarEventDto.getStatut()));
		}
		Iterable<CalendarEvent> iterable = null;
		// find calendarEvent by sysdate
		if (Boolean.TRUE.equals(calendarEventDto.getSortedByDate())) {
			OrderSpecifier<Date> orderSpecifier = qCalendarEvent.dateDebut.asc();
			predicate.and(qCalendarEvent.dateDebut.after(DateUtil.resetTimeDate(new Date())));
			predicate.or(qCalendarEvent.dateDebut.eq(DateUtil.resetTimeDate(new Date())));
			iterable = calendarEventRepository.findAll(predicate, orderSpecifier);
		}
		else {
			iterable = calendarEventRepository.findAll(predicate);
		}
		List<CalendarEvent> calendarEvents = new ArrayList<>();
		iterable.forEach(calendarEvents::add);
		// mapping data
		List<CalendarEventDTO> calendarEventDTOs = new ArrayList<>();
		calendarEvents.forEach(calendarEvent -> calendarEventDTOs
				.add(mapper.map(calendarEvent, CalendarEventDTO.class)));
		logger.info("{} : calendarEvent was founded", calendarEvents.size());
		return calendarEventDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventServiceImp#save(com.acm.utils.dtos.CalendarEventDTO)
	 */
	@Override
	public CalendarEventDTO save(CalendarEventDTO calendarEventDto) {

		Preconditions.checkNotNull(calendarEventDto,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CalendarEvent calendarEvent = mapper.map(calendarEventDto, CalendarEvent.class);
		CommonFunctions.mapperToSave(calendarEvent, userClient, logger);

		if (ACMValidationUtils.isNullOrEmpty(calendarEventDto.getUserEmail())) {
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			calendarEventDto.setUserEmail(userDTO.getEmail());
		}
		// find connected user
		if (ACMValidationUtils.isNullOrEmpty(calendarEventDto.getUsername())) {
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			calendarEvent.setUsername(userDTO.getLogin());
		}
		calendarEvent.setStatut(ACMConstantWorkflowStatuts.TASK_STATUS_NEW);
		CalendarEvent newCalendarEvent = calendarEventRepository.save(calendarEvent);
		try {
			reportingClient.sendMetingEmail(calendarEventDto);

		}
		catch (Exception e) {
			logger.error("Error while Sendig mail for the meeting = {}", e.getMessage());
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, CalendarEvent.class.getSimpleName());
		return mapper.map(newCalendarEvent, CalendarEventDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventServiceImp#save(java.lang.Integer,
	 * com.acm.utils.dtos.CalendarEventDTO)
	 */
	@Override
	public CalendarEventDTO save(Long id, CalendarEventDTO calendarEventDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(calendarEventDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update CalendarEvent  with ID = {}", id);
		CalendarEvent oldCalendarEvent = calendarEventRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCalendarEvent == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, CalendarEvent.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + CalendarEvent.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldCalendarEvent)
		mapper.map(calendarEventDTO, oldCalendarEvent);
		// mapper to update
		CommonFunctions.mapperToUpdate(oldCalendarEvent, userClient, logger);
		CalendarEvent newCalendarEvent = calendarEventRepository.save(oldCalendarEvent);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, CalendarEvent.class.getSimpleName());
		return mapper.map(newCalendarEvent, CalendarEventDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#delete(com.acm.utils.dtos.CalendarEventDTO)
	 */
	@Override
	public void delete(CalendarEventDTO calendarEventDTO) {

		Preconditions.checkNotNull(calendarEventDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(calendarEventDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete calendarEvent  with ID = {}", calendarEventDTO.getId());
		// delete object by id
		calendarEventRepository.deleteById(calendarEventDTO.getId());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, CalendarEvent.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#updateStatusTask(java.lang.Long)
	 */
	@Override
	public void updateStatusTask(Long idCollection) {

		Preconditions.checkNotNull(idCollection, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		CalendarEvent calendarEvent = new CalendarEvent();
		calendarEvent.setStatut(ACMConstantWorkflowStatuts.TASK_STATUS_CLOSED);
		calendarEvent.setIdCollection(idCollection);
		// mapper to update
		CommonFunctions.mapperToUpdate(calendarEvent, userClient, logger);
		// close old task
		int nbTaskClosed = calendarEventRepository.closeOldTaskByIdCollectionAndNotTypeEvent(
				calendarEvent.getStatut(), calendarEvent.getAcmVersion(),
				calendarEvent.getUpdatedBy(), calendarEvent.getIdCollection(),
				CommonConstants.STEP_TASK_TYPE);
		logger.info("{} tasks has been successfully closed in ACM data base", nbTaskClosed);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#saveAll(java.util.List)
	 */
	@Override
	public List<CalendarEventDTO> saveAll(List<CalendarEventDTO> listCalendarEventDTOs) {

		List<CalendarEventDTO> insertedtasksDtos = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(listCalendarEventDTOs)) {
			listCalendarEventDTOs.forEach(taskDTO -> insertedtasksDtos.add(save(taskDTO)));
			logger.info(" {} tasks was inserted.", insertedtasksDtos.size());
		}
		return insertedtasksDtos;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#closeTasksForGrpOfUsers(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public void closeTasksForGrpOfUsers(Long idCollection, String username) {

		Preconditions.checkNotNull(idCollection, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		CalendarEvent calendarEvent = new CalendarEvent();
		calendarEvent.setStatut(ACMConstantWorkflowStatuts.TASK_STATUS_CLOSED);
		calendarEvent.setIdCollection(idCollection);
		// mapper to update
		calendarEvent.setUpdatedBy("Collection Batch");
		calendarEvent.setDateLastUpdate(new Date());
		calendarEvent.setAcmVersion(
				calendarEvent.getAcmVersion() != null ? calendarEvent.getAcmVersion() + 1 : 0);
		int nbTaskClosed = calendarEventRepository.closeTasksForGrpOfUsers(
				calendarEvent.getStatut(), calendarEvent.getAcmVersion(),
				calendarEvent.getUpdatedBy(), calendarEvent.getIdCollection(), username);
		logger.info("{} tasks has been successfully closed in ACM data base", nbTaskClosed);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#closeTask(com.acm.utils.dtos.CalendarEventDTO)
	 */
	@Override
	public void closeTask(CalendarEventDTO calendarEventDTO) {

		try {
			CalendarEvent calendarEvent = mapper.map(calendarEventDTO, CalendarEvent.class);
			calendarEvent.setEnabled(true);
			CommonFunctions.mapperToUpdate(calendarEvent, userClient, logger);
			calendarEvent.setStatut(ACMConstantWorkflowStatuts.TASK_STATUS_CLOSED);
			calendarEventRepository.save(calendarEvent);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, CalendarEvent.class.getSimpleName());
		}
		catch (Exception e) {
			logger.error("Error while closing Task for the given calendar event = {}",
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarEventService#CreateLoanTaskAndCloseOldTask(com.acm.utils.dtos.
	 * CalendarEventDTO, java.lang.Long, java.lang.Boolean, java.lang.Boolean, java.lang.Boolean)
	 */
	@Override
	public void CreateLoanTaskAndCloseOldTask(CalendarEventDTO calendarEventDto, Long idLoanExtern,
			Boolean createTask, Boolean caseAssginUserLoanToHimSelf, Boolean closeAll)
			throws ResourcesNotFoundException {

		List<CalendarEventDTO> listCalendarEventDTOs = new ArrayList<>();
		CalendarEventDTO calendarEventDtoToClosed = new CalendarEventDTO();
		if (!ACMValidationUtils.isNullOrEmpty(idLoanExtern)) {
			calendarEventDtoToClosed.setIdLoanExtern(idLoanExtern);
		}
		else {
			calendarEventDtoToClosed.setIdItem(calendarEventDto.getIdItem());

		}
		if (Boolean.FALSE.equals(closeAll)) {
			calendarEventDtoToClosed.setTypeEvent(ACMConstantWorkflowStatuts.NEXT_ACTION_TASK);
		}

		calendarEventDtoToClosed.setStatut(ACMConstantWorkflowStatuts.TASK_STATUS_NEW);
		// find listCalendarEvent by idLoanExtern and typeEvent and statut
		listCalendarEventDTOs = find(calendarEventDtoToClosed);
		// check if list not empty and not null
		if (!ACMValidationUtils.isNullOrEmpty(listCalendarEventDTOs)) {
			listCalendarEventDTOs.forEach(calendarEventDTO -> {
				// if we are in the simple case
				if (Boolean.FALSE.equals(caseAssginUserLoanToHimSelf)) {
					// close task
					closeTask(calendarEventDTO);
				}
				// if we are in the case of the user assgin the loan to him self
				// we must close tasks for others users
				if (Boolean.TRUE.equals(caseAssginUserLoanToHimSelf)) {
					if (!calendarEventDTO.getUsername().equals(calendarEventDto.getUsername())) {
						// close task
						closeTask(calendarEventDTO);
					}
				}
			});
		}
		// check if we are in the case of create task
		if (Boolean.TRUE.equals(createTask)) {
			// save new tasks
			save(calendarEventDto);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.CalendarEventService#find(com.acm.utils.dtos.pagination.EventPaginationDTO)
	 */
	@Override
	public EventPaginationDTO find(EventPaginationDTO eventPaginationDTO) {

		Preconditions.checkNotNull(eventPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getPageNumber())) {
			eventPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getPageSize())) {
			eventPaginationDTO.setPageSize(10);
		}
		// setting default data
		eventPaginationDTO.setResultsEvents(new ArrayList<>());
		// setting default totals pages
		eventPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		eventPaginationDTO.setTotalPages(0);
		// init QCalendarEvent
		QCalendarEvent qCalendarEvent = QCalendarEvent.calendarEvent;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		if (Boolean.TRUE.equals(eventPaginationDTO.getParams().getAllTeamsTasks())) {
			// find calendarEvent only for connected user
			UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
			UserDTO userDTOPram = new UserDTO();
			userDTOPram.setResponsableId(userDTO.getLogin());
			List<UserDTO> teamUserDTOs = userClient.find(userDTOPram);
			List<String> teamLogins = new ArrayList<>();
			teamUserDTOs.forEach(d -> teamLogins.add(d.getLogin()));
			// find calendarEvent of connected user and of all his user's team
			predicate.and(qCalendarEvent.username.in(teamLogins));
		}
		else if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getUsername())) {
			predicate.and(qCalendarEvent.username.eq(eventPaginationDTO.getParams().getUsername()));
		}
		// find calendarEvent by extern loan id
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getIdLoanExtern())) {
			predicate.and(qCalendarEvent.idLoanExtern
					.eq(eventPaginationDTO.getParams().getIdLoanExtern()));
		}
		// find calendarEvent by collection id
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getIdCollection())) {
			predicate.and(qCalendarEvent.idCollection
					.eq(eventPaginationDTO.getParams().getIdCollection()));
		}
		// find calendarEvent by claim id
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getIdClaim())) {
			predicate.and(qCalendarEvent.idClaim.eq(eventPaginationDTO.getParams().getIdClaim()));
		}
		// find calendarEvent by category
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getCategory())) {
			predicate.and(qCalendarEvent.category.eq(eventPaginationDTO.getParams().getCategory()));
		}
		// find by Insert date
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getDateDebut())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(eventPaginationDTO.getParams().getDateDebut(), "00:00:00");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(eventPaginationDTO.getParams().getDateDebut(), "23:59:59");
			predicate.and(qCalendarEvent.dateDebut.between(StartTimesTamp, EndTimesTamp));
		}
		// find LIKE typeEvent
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getTypeEvent())) {
			predicate.and(qCalendarEvent.typeEvent
					.like("%" + eventPaginationDTO.getParams().getTypeEvent() + "%"));
		}
		// find LIKE title
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getLibelleEvent())) {
			predicate.and(qCalendarEvent.libelleEvent
					.like("%" + eventPaginationDTO.getParams().getLibelleEvent() + "%"));
		}
		// find LIKE UserFullName
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getUserFullName())) {
			predicate.and(qCalendarEvent.userFullName
					.like("%" + eventPaginationDTO.getParams().getUserFullName() + "%"));
		}
		// find LIKE Inserted By
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getInsertBy())) {
			predicate.and(qCalendarEvent.insertBy
					.like("%" + eventPaginationDTO.getParams().getInsertBy() + "%"));
		}
		// find by End date
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getDateFin())) {
			Timestamp StartTimesTamp = DateUtil
					.dateToDateTime(eventPaginationDTO.getParams().getDateFin(), "00:00:00");
			Timestamp EndTimesTamp = DateUtil
					.dateToDateTime(eventPaginationDTO.getParams().getDateFin(), "23:59:59");
			predicate.and(qCalendarEvent.dateFin.between(StartTimesTamp, EndTimesTamp));
		}
		// find LIKE customerName
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getCustomerName())) {

			predicate.and(qCalendarEvent.customerName
					.like("%" + eventPaginationDTO.getParams().getCustomerName() + "%"));
		}
		// find LIKE status
		if (!ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getParams().getStatut())) {

			predicate.and(qCalendarEvent.statut
					.like("%" + eventPaginationDTO.getParams().getStatut() + "%"));
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;

		// default sort by dateInsertion : DESC
		if ("1".equals(eventPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getSortField())) {

			pageable = PageRequest.of(eventPaginationDTO.getPageNumber(),
					eventPaginationDTO.getPageSize(), Sort.Direction.ASC,
					eventPaginationDTO.getSortField());
		}
		else if ("-1".equals(eventPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(eventPaginationDTO.getSortField())) {

			pageable = PageRequest.of(eventPaginationDTO.getPageNumber(),
					eventPaginationDTO.getPageSize(), Sort.Direction.DESC,
					eventPaginationDTO.getSortField());
		}
		else {
			// default sort by applyDate : DESC
			pageable = PageRequest.of(eventPaginationDTO.getPageNumber(),
					eventPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<CalendarEvent> pagedResult = calendarEventRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<CalendarEvent> events = pagedResult.getContent();
			List<CalendarEventDTO> calendarEventDTO = new ArrayList<>();
			// mapping data
			events.forEach(event -> {
				CalendarEventDTO dto = mapper.map(event, CalendarEventDTO.class);

				calendarEventDTO.add(dto);
			});

			logger.info("{} : Events were found (PageNumber = {} / PageSize = {} )", events.size(),
					eventPaginationDTO.getPageNumber(), eventPaginationDTO.getPageSize());
			// setting data
			eventPaginationDTO.setResultsEvents(calendarEventDTO);
			// setting totals pages
			eventPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			eventPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return eventPaginationDTO;
	}
}
