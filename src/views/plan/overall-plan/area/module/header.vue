<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        @change="monomerChange"
        @getCurrentInfo="getCurrentInfo"
      />
      <common-radio-button
        v-model="query.productType"
        :options="typeOption"
        :type="'other'"
        :dataStructure="typeProp"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <template v-if="query.productType">
      <crudOperation>
        <template #viewLeft>
          <common-button type="primary" size="mini" @click.stop="goPlanView" v-permission="crud.permission.planList">办理计划</common-button>
        </template>
      </crudOperation>
    </template>
  </div>
</template>

<script setup>
import { defineProps, ref, defineEmits } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { monomerDetail } from '@/api/plan/monomer'
import { TechnologyTypeAllEnum, projectTypeEnum } from '@enum-ms/contract'

const router = useRouter()

const defaultQuery = {
  monomerId: undefined,
  productType: undefined
}

const monomerSelectRef = ref()
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const typeOption = ref([])
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
const emit = defineEmits(['monomerChangeType', 'monomerValChange'])

async function monomerChange() {
  try {
    if (crud.query.monomerId) {
      await getTypeInfo()
    } else {
      typeOption.value = []
      emit('monomerChangeType', typeOption.value)
    }
  } catch (e) {
    console.log(e)
  } finally {
    crud.toQuery()
  }
}

function getCurrentInfo(val) {
  emit('monomerValChange', val)
}

async function getTypeInfo() {
  typeOption.value = []
  try {
    const { productTypeList } = await monomerDetail(crud.query.monomerId)
    const option = []
    if (productTypeList && productTypeList.length > 0) {
      productTypeList.forEach(v => {
        if (v.no === TechnologyTypeAllEnum.STRUCTURE.V) {
          option.unshift(v)
        } else {
          option.push(v)
        }
      })
      typeOption.value = option
    }
    crud.query.productType = typeOption.value.length > 0 ? typeOption.value[0].no : undefined
    emit('monomerChangeType', typeOption.value)
  } catch (e) {
    console.log(e)
  }
}

function goPlanView() {
  if (props.globalProject.projectType === projectTypeEnum.STEEL.V) {
    router.push({ name: 'PlanMakeManage', params: { monomerId: crud.query.monomerId, productType: crud.query.productType }})
  } else if (props.globalProject.projectType === projectTypeEnum.BRIDGE.V) {
    router.push({ name: 'BridgeMakeManage', params: { monomerId: crud.query.monomerId, productType: crud.query.productType }})
  }
}

</script>
