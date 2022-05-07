import { getArtifactSearch } from '@/api/bim/model'
import { ref } from 'vue'

export default function useArtifactSearch({ props, addBlinkByIds, removeBlink }) {
  const inputDom = ref()
  const elementIds = ref([])

  function createSearchHtml() {
    const _el = document.getElementsByClassName('bf-container')[0]
    const _html = document.createElement('div')
    _html.innerHTML = `
      <div class="bf-artifact-search">
        <div class="el-input el-input--small" style="margin-right: 10px;">
          <input class="el-input__inner bf-artifact-search-input" type="text" autocomplete="off" placeholder="构件编号搜索"></input> 
        </div>
        <button class="el-button el-button--primary el-button--small bf-artifact-search-btn-s" type="button">
          <span>搜索</span>
        </button>
        <button class="el-button el-button--warning el-button--small bf-artifact-search-btn-r" type="button">
          <span>重置</span>
        </button>
      </div>
    `
    _el.appendChild(_html)

    inputDom.value = document.getElementsByClassName('bf-artifact-search-input')[0]
    const searchBtnDom = document.getElementsByClassName('bf-artifact-search-btn-s')[0]
    searchBtnDom.addEventListener('click', () => {
      toSearch(inputDom.value.value)
    })
    const resetBtnDom = document.getElementsByClassName('bf-artifact-search-btn-r')[0]
    resetBtnDom.addEventListener('click', () => {
      removeBlink()
      inputDom.value.value = ''
    })
  }

  async function toSearch(serialNumber) {
    if (!serialNumber) return
    try {
      const _elementIds = await getArtifactSearch({
        serialNumber: serialNumber,
        monomerId: props.monomerId
      })
      addBlinkByIds(_elementIds)
      elementIds.value = _elementIds
    } catch (error) {
      console.log('构件编号搜索报错', error)
    }
  }

  return {
    createSearchHtml,
    searchBySN: toSearch
  }
}
