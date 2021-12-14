<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        @change="monomerChange"
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
    <crudOperation :disabled="!query.productType"/>
  </div>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { monomerDetail } from '@/api/plan/monomer'
 import { useRoute } from 'vue-router'  

const route = useRoute() 
const defaultQuery = {
  projectId:undefined,
  monomerId: undefined,
  productType: undefined
}
const monomerSelectRef = ref()
const queryMonomerId = ref()

const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const typeOption = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const emit = defineEmits(['monomerChangeType'])

watch(
  () => route.params.monomerId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      queryMonomerId.value = +route.params.monomerId
      console.log(route.params.productType)
      if (queryMonomerId.value) {
        crud.query.monomerId =  queryMonomerId.value
        crud.query.productType = route.params.productType
        monomerChange()
      }
    }
  },
  { deep: true, immediate: true }
)

async function monomerChange() {
  try {
    if (crud.query.monomerId) {
      await getTypeInfo()
    } else {
      typeOption.value = []
    }
  } catch (e) {
    console.log(e)
  } finally {
    crud.toQuery()
  }
}

async function getTypeInfo() {
  try {
    const { productTypeList } = await monomerDetail(crud.query.monomerId)
    typeOption.value = []
    const option = []
    if (productTypeList && productTypeList.length > 0) {
      productTypeList.forEach(v => {
        if (v.no === 5) {
          option.unshift(v)
        } else {
          option.push(v)
        }
      })
      typeOption.value = option
    }
    if (queryMonomerId.value === crud.query.monomerId) {
      crud.query.productType = route.params.productType
    } else {
      crud.query.productType = typeOption.value.length > 0 ? typeOption.value[0].no : undefined
    }
    console.log(crud.query.productType)
  } catch (e) {
    console.log(e)
  }
}

</script>
