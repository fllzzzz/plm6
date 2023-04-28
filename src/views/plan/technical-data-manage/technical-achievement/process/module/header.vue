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
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.processType"
        :options="planProcessTypeEnum.ENUM"
        show-option-all
        class="filter-item"
        type="enum"
        style="margin-bottom:10px;"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.fileName"
        placeholder="文件名搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <common-select
        v-model="query.structureClassId"
        :options="structureClassList"
        type="other"
        clearable
        :data-structure="{ key: 'id', label: 'name', value: 'id' }"
        class="filter-item"
        style="width: 200px"
        placeholder="构件类型"
        @change="crud.toQuery"
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
        <common-button type="warning" size="mini" @click="crud.toAdd" v-permission="crud.permission.add" class="filter-item">
          上传工艺文件
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { systemStructureClass } from '@/api/plan/technical-data-manage/process'
import { ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'

import { processUseTypeEnum, planProcessTypeEnum } from '@enum-ms/plan'
// import checkPermission from '@/utils/system/check-permission'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  boolSingleProject: undefined,
  fileName: undefined,
  material: undefined,
  name: undefined,
  processType: undefined,
  serialNumber: undefined,
  specification: undefined,
  structureClassId: undefined
}
const emit = defineEmits(['structureClassChange'])
const { crud, query } = regHeader(defaultQuery)

const structureClassList = ref([])

fetchList()

async function fetchList() {
  let _list = []
  try {
    const { content = [] } = await systemStructureClass()
    _list = content
  } catch (error) {
    console.log('系统构件类型', error)
  } finally {
    structureClassList.value = _list
    emit('structureClassChange', structureClassList.value)
  }
}
</script>
