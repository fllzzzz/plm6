<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="currentOption"
        :type="'other'"
        :dataStructure="typeProp"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
// import { planTypeEnum } from '@enum-ms/plan'

const defaultQuery = {
  productType: { value: undefined, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const currentOption = ref([])
const techOptions = [
  { name: '构件', key: 'mainStructure', dateKey: 'mainStructureDate', no: TechnologyTypeAllEnum.STRUCTURE.V, alias: 'STRUCTURE' },
  {
    name: '夹芯板',
    key: 'battenBoard',
    dateKey: 'battenBoardDate',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型板',
    key: 'contourPlate',
    dateKey: 'contourPlateDate',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    key: 'flangingPiece',
    dateKey: 'flangingPieceDate',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    key: 'trussFloorPlate',
    dateKey: 'trussFloorPlateDate',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    key: 'pressureBearingPlate',
    dateKey: 'pressureBearingPlateDate',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  }
]
watch(
  () => props.globalProject,
  (val) => {
    if (isNotBlank(val)) {
      currentOption.value = []
      val.projectContentList.forEach((v) => {
        if (val.businessType === businessTypeEnum.ENUM.MACHINING.V) {
          if (v.no && techOptions.findIndex((k) => k.no === Number(v.no)) > -1) {
            const optionVal = techOptions.find((k) => k.no === Number(v.no))
            currentOption.value.push(optionVal)
          }
        } else if (val.businessType === businessTypeEnum.ENUM.INSTALLATION.V) {
          if (v.childrenList && v.childrenList.length > 0) {
            v.childrenList.forEach((value) => {
              if (value.no && techOptions.findIndex((k) => k.no === Number(value.no)) > -1) {
                const optionVal = techOptions.find((k) => k.no === Number(value.no))
                currentOption.value.push(optionVal)
              }
            })
          }
        }
      })
      if (currentOption.value.findIndex((k) => k.alias === 'ENCLOSURE') > -1) {
        const optionVal = {
          label: '折边件(t)',
          key: 'flangingPiece',
          dateKey: 'flangingPieceDate',
          alias: 'ENCLOSURE',
          no: TechnologyTypeAllEnum.BENDING.V
        }
        currentOption.value.push(optionVal)
      }
      crud.query.productType = currentOption.value.length > 0 ? currentOption.value[0].no : undefined
    }
  },
  { deep: true, immediate: true }
)
</script>
