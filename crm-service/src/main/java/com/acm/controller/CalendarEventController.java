/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.client.UserClient;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.CalendarEventService;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.pagination.EventPaginationDTO;

// TODO: Auto-generated Javadoc
/**
 * This class @{link CalendarEventController} used to control all the CalendarEvent requests.
 *
 * @author MoezMhiri
 * @since 0.5.0
 */
@RestController
@RequestMapping("/calendar-events")
public class CalendarEventController {

	/** The calendarEvent service. */
	@Autowired
	private CalendarEventService calendarEventService;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/**
	 * Find by id.
	 *
	 * @author MoezMhiri
	 * @param id the id
	 * @return the calendarEvent DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public CalendarEventDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return calendarEventService.find(id);
	}

	/**
	 * Find {@link List} of {@link CalendarEventDTO} by Requested params.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDTO the calendar event DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<CalendarEventDTO> find(@RequestBody CalendarEventDTO calendarEventDTO) {

		return calendarEventService.find(calendarEventDTO);
	}

	/**
	 * Creates the CalendarEventDTO by new value.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDTO the Calendar Event DTO
	 * @return the Calendar Event DTO
	 */
	@PostMapping("/create")
	public CalendarEventDTO create(@RequestBody CalendarEventDTO calendarEventDTO) {

		return calendarEventService.save(calendarEventDTO);
	}

	/**
	 * Update the parameter by id.
	 * 
	 * @author MoezMhiri
	 * @param calendarEventDTO the Calendar EventDTO
	 * @return the Calendar EventDTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public CalendarEventDTO update(@RequestBody CalendarEventDTO calendarEventDTO)
			throws ResourcesNotFoundException {

		return calendarEventService.save(calendarEventDTO.getId(), calendarEventDTO);
	}

	/**
	 * Delete document using his id.
	 * 
	 * @author MoezMhiri
	 * @param id the id
	 */
	@DeleteMapping("/{id}")
	public void delete(@PathVariable("id") Long id) {

		calendarEventService.delete(new CalendarEventDTO(id));
	}

	/**
	 * Update status task.
	 * 
	 * @author idridi
	 * @param idCollection the id collection
	 */
	@PostMapping("/update-status-task")
	public void updateStatusTask(@RequestBody Long idCollection) {

		calendarEventService.updateStatusTask(idCollection);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param listCalendarEventDTOs the list calendar event DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save-all")
	public List<CalendarEventDTO> saveAll(@RequestBody List<CalendarEventDTO> listCalendarEventDTOs)
			throws ResourcesNotFoundException {

		return calendarEventService.saveAll(listCalendarEventDTOs);
	}

	/**
	 * Close tasks for grp of users.
	 *
	 * @author idridi
	 * @param idCollection the id collection
	 * @param username the username
	 */
	@PutMapping("/close-tasks-for-group-users/{idCollection}/{username}")
	public void closeTasksForGrpOfUsers(@PathVariable("idCollection") Long idCollection,
			@PathVariable("username") String username) {

		calendarEventService.closeTasksForGrpOfUsers(idCollection, username);
	}

	/**
	 * Close task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/closeTask")
	public void closeTask(@RequestBody CalendarEventDTO calendarEventDTO)
			throws ResourcesNotFoundException {

		calendarEventService.closeTask(calendarEventDTO);
	}

	/**
	 * Creates the loan task and close old task.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @param createTask the create task
	 * @param caseAssginUserLoanToHimSelf the case assgin user loan to him self
	 * @param closeAll the close all
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/create-loan-task-and-close-old-task/{createTask}/{caseAssginUserLoanToHimSelf}/{closeAll}")
	public void createLoanTaskAndCloseOldTask(@RequestBody CalendarEventDTO calendarEventDTO,
			@PathVariable("createTask") Boolean createTask,
			@PathVariable("caseAssginUserLoanToHimSelf") Boolean caseAssginUserLoanToHimSelf,
			@PathVariable("closeAll") Boolean closeAll) throws ResourcesNotFoundException {

		calendarEventService.CreateLoanTaskAndCloseOldTask(calendarEventDTO,
				calendarEventDTO.getIdLoanExtern(), createTask, caseAssginUserLoanToHimSelf,
				closeAll);
	}

	/**
	 * Find pagination.
	 *
	 * @param eventPaginationDTO the event pagination DTO
	 * @return the event pagination DTO
	 */
	@PostMapping("/find-pagination")
	public EventPaginationDTO findPagination(@RequestBody EventPaginationDTO eventPaginationDTO) {

		return calendarEventService.find(eventPaginationDTO);
	}

}
