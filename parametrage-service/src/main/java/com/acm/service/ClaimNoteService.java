/*
 * 
 */
package com.acm.service;
import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ClaimNoteDTO;

// TODO: Auto-generated Javadoc
/**
 * The Interface ClaimNoteService.
 */
public interface ClaimNoteService {
	
  
	/**
	 * Find.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<ClaimNoteDTO> find(ClaimNoteDTO claimNoteDTO) throws ResourcesNotFoundException;  
 
	/**
	 * Save.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @param categorie the categorie
	 * @return the claim note DTO
	 */
	ClaimNoteDTO save(ClaimNoteDTO claimNoteDTO, String categorie);
  
	/**
	 * Save note from ib.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @return the claim note DTO
	 */
	ClaimNoteDTO saveNoteFromIb(ClaimNoteDTO claimNoteDTO);

}
