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

import com.acm.exceptions.type.ExpensesTypeUnicityCodeException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ExpensesTypeService;
import com.acm.utils.dtos.ExpensesTypeDTO;

/**
 * This class @{link ExpensesTypeController} used to control all the Expenses Type requests.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@RestController
@RequestMapping("/expenses-type")
public class ExpensesTypeController {

	/** The expenses type service. */
	@Autowired
	private ExpensesTypeService expensesTypeService;

	/**
	 * Find all.
	 * 
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/")
	public List<ExpensesTypeDTO> find() {

		return expensesTypeService.findAll();
	}

	/**
	 * Save.
	 *
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the expenses type DTO
	 * @throws ExpensesTypeUnicityCodeException the expenses type unicity code exception
	 */
	@PostMapping("/create")
	public ExpensesTypeDTO save(@RequestBody ExpensesTypeDTO expensesTypeDTO)
			throws ExpensesTypeUnicityCodeException {

		return expensesTypeService.save(expensesTypeDTO);
	}

	/**
	 * Update.
	 * 
	 * @author YesserSomai
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the expenses type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update")
	public ExpensesTypeDTO update(@RequestBody ExpensesTypeDTO expensesTypeDTO)
			throws ResourcesNotFoundException {

		return expensesTypeService.save(expensesTypeDTO.getId(), expensesTypeDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author YesserSomai
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		expensesTypeService.delete(id);
	}

	/**
	 * Update document name.
	 *
	 * @param documentLabel the document label
	 * @param documentId the document id
	 */
	@PutMapping("/update-document-name/{documentID}")
	public void updateDocumentName(@RequestBody String documentLabel,
			@PathVariable("documentID") Long documentId) {

		expensesTypeService.updateDocumentName(documentLabel, documentId);
	}

	/**
	 * Find.
	 *
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ExpensesTypeDTO> find(@RequestBody ExpensesTypeDTO expensesTypeDTO) {

		return expensesTypeService.find(expensesTypeDTO);
	}

}
