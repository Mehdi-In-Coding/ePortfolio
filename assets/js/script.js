'use strict';

// Fonction de bascule d'élément générique.
const elementToggleFunc = function (elem) { elem.classList.toggle("active"); }

// Variables de la barre latérale.
const sidebar = document.querySelector("[data-sidebar]");
const sidebarBtn = document.querySelector("[data-sidebar-btn]");

// Ajout d'un écouteur d'événement pour basculer la barre latérale sur mobile.
sidebarBtn.addEventListener("click", function () { elementToggleFunc(sidebar); });

// Variables des témoignages et de la boîte modale.
const testimonialsItem = document.querySelectorAll("[data-testimonials-item]");
const modalContainer = document.querySelector("[data-modal-container]");
const modalCloseBtn = document.querySelector("[data-modal-close-btn]");
const overlay = document.querySelector("[data-overlay]");

// Variables de la boîte modale.
const modalImg = document.querySelector("[data-modal-img]");
const modalTitle = document.querySelector("[data-modal-title]");
const modalText = document.querySelector("[data-modal-text]");

// Fonction de bascule pour la boîte modale des témoignages.
const testimonialsModalFunc = function () {
  modalContainer.classList.toggle("active");
  overlay.classList.toggle("active");
}

// Ajout d'un événement de clic pour tous les éléments de la boîte modale.
for (let i = 0; i < testimonialsItem.length; i++) {

  testimonialsItem[i].addEventListener("click", function () {

    // Mise à jour du contenu de la boîte modale.
    modalImg.src = this.querySelector("[data-testimonials-avatar]").src;
    modalImg.alt = this.querySelector("[data-testimonials-avatar]").alt;
    modalTitle.innerHTML = this.querySelector("[data-testimonials-title]").innerHTML;
    modalText.innerHTML = this.querySelector("[data-testimonials-text]").innerHTML;

    // Affichage de la boîte modale.
    testimonialsModalFunc();

  });

}

// Ajout d'événements de clic pour le bouton de fermeture de la boîte modale.
modalCloseBtn.addEventListener("click", testimonialsModalFunc);
overlay.addEventListener("click", testimonialsModalFunc);

// Variables du sélecteur personnalisé.
const select = document.querySelector("[data-select]");
const selectItems = document.querySelectorAll("[data-select-item]");
const selectValue = document.querySelector("[data-selecct-value]");
const filterBtn = document.querySelectorAll("[data-filter-btn]");

// Ajout d'un événement de clic pour basculer le sélecteur personnalisé.
select.addEventListener("click", function () { elementToggleFunc(this); });

// Ajout d'événements de clic pour tous les éléments du sélecteur.
for (let i = 0; i < selectItems.length; i++) {
  selectItems[i].addEventListener("click", function () {

    // Mise à jour de la valeur sélectionnée et du sélecteur.
    let selectedValue = this.innerText.toLowerCase();
    selectValue.innerText = this.innerText;
    elementToggleFunc(select);
    filterFunc(selectedValue);

  });
}

// Variables du filtre.
const filterItems = document.querySelectorAll("[data-filter-item]");

// Fonction de filtre en fonction de la valeur sélectionnée.
const filterFunc = function (selectedValue) {

  for (let i = 0; i < filterItems.length; i++) {

    // Application du filtre en fonction de la catégorie sélectionnée.
    if (selectedValue === "all") {
      filterItems[i].classList.add("active");
    } else if (selectedValue === filterItems[i].dataset.category) {
      filterItems[i].classList.add("active");
    } else {
      filterItems[i].classList.remove("active");
    }

  }

}

// Ajout d'événements de clic pour tous les boutons de filtre sur grand écran.
let lastClickedBtn = filterBtn[0];

for (let i = 0; i < filterBtn.length; i++) {

  filterBtn[i].addEventListener("click", function () {

    // Mise à jour de la valeur sélectionnée et du sélecteur.
    let selectedValue = this.innerText.toLowerCase();
    selectValue.innerText = this.innerText;
    filterFunc(selectedValue);

    // Gestion de la mise en surbrillance du dernier bouton cliqué.
    lastClickedBtn.classList.remove("active");
    this.classList.add("active");
    lastClickedBtn = this;

  });

}

// Variables du formulaire de contact.
const form = document.querySelector("[data-form]");
const formInputs = document.querySelectorAll("[data-form-input]");
const formBtn = document.querySelector("[data-form-btn]");

// Ajout d'un événement pour tous les champs de saisie du formulaire.
for (let i = 0; i < formInputs.length; i++) {
  formInputs[i].addEventListener("input", function () {

    // Vérification de la validation du formulaire.
    if (form.checkValidity()) {
      formBtn.removeAttribute("disabled");
    } else {
      formBtn.setAttribute("disabled", "");
    }

  });
}

// Variables de navigation de page.
const navigationLinks = document.querySelectorAll("[data-nav-link]");
const pages = document.querySelectorAll("[data-page]");

// Ajout d'événements de clic pour tous les liens de navigation.
for (let i = 0; i < navigationLinks.length; i++) {
  navigationLinks[i].addEventListener("click", function () {

    // Gestion de l'affichage des pages en fonction du lien de navigation cliqué.
    for (let i = 0; i < pages.length; i++) {
      if (this.innerHTML.toLowerCase() === pages[i].dataset.page) {
        pages[i].classList.add("active");
        navigationLinks[i].classList.add("active");
        window.scrollTo(0, 0);
      } else {
        pages[i].classList.remove("active");
        navigationLinks[i].classList.remove("active");
      }
    }

  });
}
