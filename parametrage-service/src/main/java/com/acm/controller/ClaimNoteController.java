/*
 * 
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ClaimNoteService;
import com.acm.utils.dtos.ClaimNoteDTO;

// TODO: Auto-generated Javadoc
/**
 * The Class ClaimNoteController.
 */
@RestController
@RequestMapping("/claim-note")
public class ClaimNoteController {

	/** The claim note service. */
	@Autowired
	private ClaimNoteService claimNoteService;
	
	/**
	 * Find.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/")
	public List<ClaimNoteDTO> find(@RequestBody ClaimNoteDTO claimNoteDTO)throws ResourcesNotFoundException {
		
		return claimNoteService.find(claimNoteDTO);
	}
	
	/**
	 * Creates the.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @param categorie the categorie
	 * @return the claim note DTO
	 */
	@PostMapping("/create/{categorie}")
	public ClaimNoteDTO create(@RequestBody ClaimNoteDTO claimNoteDTO, @PathVariable String categorie) {
		return claimNoteService.save(claimNoteDTO, categorie);
	}
	
	/**
	 * Save note from ib.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the claim note DTO
	 */
	@PostMapping("/save-note-acm")
	public ClaimNoteDTO saveNoteFromIb(@RequestBody ClaimNoteDTO claimNoteDTO) {
		return claimNoteService.saveNoteFromIb(claimNoteDTO);
	}

}
