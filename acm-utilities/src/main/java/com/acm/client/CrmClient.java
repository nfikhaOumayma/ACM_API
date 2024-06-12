/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.util.List;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.CalendarEventDTO;

/**
 * {@link CrmClient} CRM Client.
 *
 * @author idridi
 * @since 1.1.9
 */
@FeignClient(value = "crm-service", configuration = ClientConfiguration.class, decode404 = true)
@RibbonClient(name = "crm-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface CrmClient {

	/**
	 * Creates the.
	 *
	 * @author idridi
	 * @param calendarEventDTO the calendar event DTO
	 * @return the calendar event DTO
	 */
	@PostMapping("/calendar-events/create")
	CalendarEventDTO create(@RequestBody CalendarEventDTO calendarEventDTO);

	/**
	 * Update status task.
	 * 
	 * @author idridi
	 * @param idCollection the id collection
	 */
	@PostMapping("/calendar-events/update-status-task")
	void updateStatusTask(@RequestBody Long idCollection);

	/**
	 * Close tasks for closed collections.
	 *
	 * @author idridi
	 * @param token the token
	 */
	@PostMapping("/calendar-events/close-tasks-closed-collections")
	void closeTasksForClosedCollections(@RequestHeader("Authorization") String token);

	/**
	 * Save all.
	 * 
	 * @author idridi
	 * @param listCalendarEventDTOs the list calendar event DT os
	 * @return the list
	 */
	@PostMapping("/calendar-events/save-all")
	List<CalendarEventDTO> saveAll(@RequestBody List<CalendarEventDTO> listCalendarEventDTOs);

	/**
	 * Close tasks for grp of users.
	 * 
	 * @author idridi
	 * @param idCollection the id collection
	 * @param username the username
	 */
	@PutMapping("/calendar-events/close-tasks-for-group-users/{idCollection}/{username}")
	void closeTasksForGrpOfUsers(@PathVariable("idCollection") Long idCollection,
			@PathVariable("username") String username);

	/**
	 * Creates the loan task and close old task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @param createTask the create task
	 * @param caseAssginUserLoanToHimSelf the case assgin user loan to him self
	 * @param closeAll the close all
	 */
	@PostMapping("/calendar-events/create-loan-task-and-close-old-task/{createTask}/{caseAssginUserLoanToHimSelf}/{closeAll}")
	void createLoanTaskAndCloseOldTask(@RequestBody CalendarEventDTO calendarEventDTO,
			@PathVariable("createTask") Boolean createTask,
			@PathVariable("caseAssginUserLoanToHimSelf") Boolean caseAssginUserLoanToHimSelf,
			@PathVariable("closeAll") Boolean closeAll);

	/**
	 * Close task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 */
	@PutMapping("/calendar-events/closeTask")
	void closeTask(@RequestBody CalendarEventDTO calendarEventDTO);

	/**
	 * Find task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @return the list
	 */
	@PostMapping("/calendar-events/")
	List<CalendarEventDTO> findTask(@RequestBody CalendarEventDTO calendarEventDTO);
}
