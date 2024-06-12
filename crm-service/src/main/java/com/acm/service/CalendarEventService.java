/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.pagination.EventPaginationDTO;

// TODO: Auto-generated Javadoc
/**
 * {@link CalendarEventService } interface.
 *
 * @author MoezMhiri
 * @since 0.5.0
 */
public interface CalendarEventService {

	/**
	 * Find {@link CalendarEventDTO} by given ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the CalendarEvent DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CalendarEventDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link CalendarEventDTO} by given params.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDto the calendarEvent DTO
	 * @return the list
	 */
	List<CalendarEventDTO> find(CalendarEventDTO calendarEventDto);

	/**
	 * The method used for saving the given {@link CalendarEventDTO}.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDto the calendarEvent DTO
	 * @return the calendarEvent DTO
	 */
	CalendarEventDTO save(CalendarEventDTO calendarEventDto);

	/**
	 * The method used for updating the given {@link calendarEventDTO} by ID.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @param calendarEventDTO the calendar event DTO
	 * @return the calendar event DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	CalendarEventDTO save(Long id, CalendarEventDTO calendarEventDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete {@link calendarEventDTO} by given params.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDTO the calendar event DTO
	 */
	void delete(CalendarEventDTO calendarEventDTO);

	/**
	 * Update status task.
	 * 
	 * @author idridi
	 * @param idCollection the id collection
	 */
	void updateStatusTask(Long idCollection);

	/**
	 * Save all.
	 * 
	 * @author idridi
	 * @param listCalendarEventDTOs the list calendar event DT os
	 * @return the list
	 */
	List<CalendarEventDTO> saveAll(List<CalendarEventDTO> listCalendarEventDTOs);

	/**
	 * Close tasks for grp of users. àau
	 *
	 * @param idCollection the id collection
	 * @param username the username
	 */
	void closeTasksForGrpOfUsers(Long idCollection, String username);

	/**
	 * Close task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void closeTask(CalendarEventDTO calendarEventDTO) throws ResourcesNotFoundException;

	/**
	 * Creates the loan task and close old task.
	 *
	 * @param calendarEventDto the calendar event dto
	 * @param idLoanExtern the id loan extern
	 * @param createTask the create task
	 * @param caseAssginUserLoanToHimSelf the case assgin user loan to him self
	 * @param closeAll the close all
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void CreateLoanTaskAndCloseOldTask(CalendarEventDTO calendarEventDto, Long idLoanExtern,
			Boolean createTask, Boolean caseAssginUserLoanToHimSelf, Boolean closeAll)
			throws ResourcesNotFoundException;

	/**
	 * Find.
	 *
	 * @param eventDTO the event DTO
	 * @return the event pagination DTO
	 */
	EventPaginationDTO find(EventPaginationDTO eventDTO);
}
