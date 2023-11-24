<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.category"
        :options="typeOption"
        :type="'other'"
        showOptionAll
        :dataStructure="{ key: 'no', label: 'name', value: 'no' }"
        class="filter-item"
        @change="categoryChange"
      />
      <template v-if="query.category">
        <template v-if="isNotBlank(areaInfo)">
          <common-select
            v-model="query.enclosurePlanId"
            :options="areaInfo"
            type="other"
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            size="small"
            clearable
            placeholder="请选择围护计划"
            class="filter-item"
            style="width:200px;"
            @change="crud.toQuery"
          />
        </template>
        <template v-else>
          <span style="font-size:12px;color:red;margin-left:10px;">*当前项目下未创建计划</span>
        </template>
      </template>
      <common-select
        type="enum"
        v-model="query.boolReturn"
        :options="whetherEnum.ENUM"
        clearable
        placeholder="是否退量"
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation :disabled="!query.enclosurePlanId">
       <template #optRight>
        <upload-btn
          v-if="crud.query.projectId && checkPermission(crud.permission.import)"
          :data="uploadQuery"
          :upload-fun="enclosureStandardPartUpload"
          btn-name="清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          :disabled="!query.enclosurePlanId"
          @success="crud.toQuery()"
        />
        <export-button :fn="enclosureDownloadStandardPart" class="filter-item" v-permission="crud.permission.templateDownload">
          清单模板
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { isNotBlank } from '@data-type/index'
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'
import { enclosureStandardPartUpload, enclosureDownloadStandardPart } from '@/api/plan/technical-manage/standard-part'
import { defineProps, watch, computed, ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'

import { whetherEnum } from '@enum-ms/common'

import crudOperation from '@crud/CRUD.operation'
// import monomerSelect from '@/components-system/plan/monomer-select'
// import rrOperation from '@crud/RR.operation'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  boolReturn: undefined
}

const emit = defineEmits(['enclosurePlan'])

const areaInfo = ref([])
const originAreaInfo = ref([])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  typeOption: {
    type: Array,
    default: undefined
  }
})
const uploadQuery = computed(() => {
  const obj = {}
  for (const i in query) {
    if (query[i]) {
      obj[i] = query[i]
    }
  }
  return obj
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      query.enclosurePlanId = undefined
      query.category = undefined
      getAllProjectPlan()
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

async function getAllProjectPlan() {
  if (props.projectId) {
    try {
      const data = await allProjectPlan(props.projectId) || []
      originAreaInfo.value = data
      emit('enclosurePlan', originAreaInfo.value)
      areaInfo.value = data.filter(v => v.category === crud.query.category) || []
      query.enclosurePlanId = undefined
      // if (areaInfo.value && areaInfo.value.length > 0) {
      //   query.enclosurePlanId = areaInfo.value[0].id
      // }
      crud.toQuery()
    } catch (e) {
      console.log('获取项目所有计划', e)
    }
  }
}

function categoryChange(val) {
  query.enclosurePlanId = undefined
  areaInfo.value = originAreaInfo.value.filter(v => v.category === crud.query.category) || []
  // if (areaInfo.value && areaInfo.value.length > 0) {
  //   query.enclosurePlanId = areaInfo.value[0].id
  // }
  crud.toQuery()
}
</script>
