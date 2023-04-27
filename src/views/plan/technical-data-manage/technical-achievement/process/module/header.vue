<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.boolSingleProject"
        :options="processUseTypeEnum.ENUM"
        show-option-all
        class="filter-item"
        type="enum"
        style="margin-bottom:10px;"
      />
      <common-radio-button
        v-model="query.processType"
        :options="planProcessTypeEnum.ENUM"
        show-option-all
        class="filter-item"
        type="enum"
        style="margin-bottom:10px;"
      />
      <el-input
        v-model="query.fileName"
        placeholder="文件名搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <el-input
        v-model="query.structureClassId"
        placeholder="构件类型搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <el-input
        v-model="query.name"
        placeholder="构件名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="构件编号"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <el-input
        v-model="query.specification"
        placeholder="构件规格"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <el-input
        v-model="query.material"
        placeholder="构件材质"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation :permission="crud.permission">
      <template #optLeft>
        <common-button type="warning" size="mini" @click="crud.toAdd" v-permission="crud.permission.import" class="filter-item">
          上传工艺文件
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'

import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
// import checkPermission from '@/utils/system/check-permission'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  // projectId: undefined,
  boolSingleProject: undefined,
  fileName: undefined,
  material: undefined,
  name: undefined,
  processType: undefined,
  serialNumber: undefined,
  specification: undefined,
  structureClassId: undefined
}

const emit = defineEmits(['currentChange', 'handleUpload'])
const { crud, query } = regHeader(defaultQuery)
// const props = defineProps({
//   projectId: {
//     type: [Number, String],
//     default: undefined
//   }
// })

</script>
