package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.GenericWorkflowObjectService;
import com.acm.utils.dtos.GenericWorkFlowObjectDTO;

/**
 * The Class GenericWorkFlowObjectController.
 */
@RestController
@RequestMapping("/generic-workflow-object")
public class GenericWorkFlowObjectController {

	/** The generic workflow service. */
	@Autowired
	private GenericWorkflowObjectService genericWorkflowService;

	/**
	 * Creates the.
	 *
	 * @param genericWorkFlowObjectDTO the generic work flow object DTO
	 * @return the generic work flow object DTO
	 */
	@PostMapping("/create")
	public GenericWorkFlowObjectDTO create(
			@RequestBody GenericWorkFlowObjectDTO genericWorkFlowObjectDTO) {

		return genericWorkflowService.save(genericWorkFlowObjectDTO);
	}

	/**
	 * Find all.
	 *
	 * @return the list
	 */
	@GetMapping("/findAll")
	public List<GenericWorkFlowObjectDTO> findAll() {

		return genericWorkflowService.findAll();
	}

}
