<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :default="monomerDefault"
        :defaultValue="queryMonomerId"
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
      <common-radio-button
        v-model="query.type"
        :options="areaPlanTypeEnum.ENUM"
        :disabled-val="disabledVal"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation :disabled="!query.productType"/>
  </div>
</template>

<script setup>
import { defineProps, ref, computed, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { monomerDetail } from '@/api/plan/monomer'
import { useRoute } from 'vue-router'
import { areaPlanTypeEnum } from '@enum-ms/plan'
import { businessTypeEnum } from '@enum-ms/contract'

const route = useRoute()
const defaultQuery = {
  monomerId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false },
  type: { value: areaPlanTypeEnum.DEEPEN.V, resetAble: false }
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
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.projectId,
  (val) => {
    queryMonomerId.value = undefined
  },
  { immediate: true, deep: true }
)

const disabledVal = computed(() => {
  return props.globalProject.businessType === businessTypeEnum.MACHINING.V ? [areaPlanTypeEnum.INSTALL.V] : []
})
const monomerDefault = computed(() => {
  return !queryMonomerId.value
})
if (route.params.monomerId) {
  queryMonomerId.value = +route.params.monomerId
  if (queryMonomerId.value) {
    crud.query.monomerId = queryMonomerId.value
    monomerChange()
  }
}

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
  } catch (e) {
    console.log(e)
  }
}

</script>
