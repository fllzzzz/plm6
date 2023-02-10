<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
           <!-- <factory-select
           v-model="query.factoryId"
           placeholder="请选择工厂"
           class="filter-item"
           style="width: 270px"
           :factory-id="query.factoryId"/> -->
           <!-- @change="crud.toQuery"  -->
          <!-- <workshop-select
          v-model="query.workshopInfId"
          placeholder="请先选择车间"
          style="width: 270px"
          class="filter-item"
          :factory-id="query.factoryId"
          /> -->
          <!-- <production-line-select
          v-model="query.productionLineId"
          :factory-id="query.factoryId"
          placeholder="请先选择生产线"
          class="filter-item"
          style="width: 270px"/> -->
          <!-- <rrOperation /> -->
          <el-cascader
            :options="subList"
            check-strictly
            v-model="query.copyValue"
            :props="cascaderProps"
            separator=" > "
            show-all-levels
            clearable
            size="small"
            class="filter-item"
            style="width: 300px"
            placeholder="可选择工厂、车间、生产线"
            @change="crud.toQuery"
          />
          <common-button class="filter-item" size="mini" type="primary">确定</common-button>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
// import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
// import factorySelect from '@comp-base/factory-select.vue'
// import workshopSelect from '@/components-system/base/workshop-select.vue'
// import productionLineSelect from '@comp-mes/production-line-select'
import { getAllFactoryWorkshopLines } from '@/api/mes/common'
import { defineEmits, defineProps, computed, watch, ref, onMounted } from 'vue'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: [Array, Number]
  },
  checkStrictly: {
    // 启用该功能后，可让父子节点取消关联，选择任意一级选项。
    type: Boolean,
    default: false
  },
  expandTrigger: {
    // 次级菜单的展开方式
    type: String,
    default: 'hover'
  },
  emitPath: {
    type: Boolean,
    default: false
  },
  filterable: {
    type: Boolean,
    default: false
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  showAllLevels: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择工厂、车间和生产线'
  }
})
const copyValue = ref()
const subList = ref([])
const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    checkStrictly: props.checkStrictly,
    expandTrigger: props.expandTrigger,
    emitPath: props.emitPath,
    multiple: props.multiple
  }
})
watch(
  () => props.modelValue,
  (value) => {
    if (value instanceof Array) {
      copyValue.value = [...value]
    } else {
      copyValue.value = value
    }
    handleChange(value)
  },
  { immediate: true }
)
const defaultQuery = {
  factoryId: undefined,
  workshopInfId: undefined,
  copyValue: undefined
}
onMounted(() =>
  allFactoryWorkshopLines()
)

async function allFactoryWorkshopLines() {
  try {
    const { content } = await getAllFactoryWorkshopLines({})
    content.forEach(v => {
      v.children = v.workshopList
      v.workshopList.forEach(p => {
        p.children = p.productionLineList
      })
    })
    console.log(content)
    subList.value = content
  } catch (error) {
    console.log('请求工厂-车间-生产线的层级接口失败')
  }
}
function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
const { crud, query } = regHeader(defaultQuery)
</script>
