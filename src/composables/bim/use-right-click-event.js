import { ref } from 'vue'

export default function useRightClickEvent({ fetchArtifactInfo }) {
  const rightMenuDom = ref()

  function addRightEventListener({ viewer, viewer3DEvent }) {
    console.log(viewer3DEvent, 'addRightEventListener')
    viewer.value.addEventListener(viewer3DEvent.value.ContextMenu, (objectData) => {
      console.log(viewer.value.getSelectedComponents(), objectData, 'objectData')
      const selectedIds = viewer.value.getSelectedComponents()
      if (objectData.objectType === 'Component' && selectedIds && selectedIds.length) {
        setTimeout(function () {
          addContextMenu(viewer, selectedIds[0])
        }, 0)
      }
    })
  }

  function addContextMenu(viewer, elementId) {
    const artifactInfoMenu = document.createElement('div')
    artifactInfoMenu.className = 'bf-menu-item user-item'
    artifactInfoMenu.innerHTML = '构件信息'
    rightMenuDom.value = document.querySelector('.bf-menu.bf-menu-right')
    rightMenuDom.value.insertBefore(artifactInfoMenu, rightMenuDom.value.children[0])
    console.log(rightMenuDom.value)
    artifactInfoMenu.addEventListener('click', () => {
      fetchArtifactInfo(elementId)
      rightMenuDom.value.style.display = 'none'
    })
  }

  return {
    addRightEventListener
  }
}
