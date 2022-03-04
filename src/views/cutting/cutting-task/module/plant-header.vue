<template>
  <div v-show="crud.searchToggle">
    <el-input
      v-model="query.machineName"
      placeholder="输入机器名称搜索"
      class="filter-item"
      style="width: 150px"
      size="small"
      clearable
      @blur="crud.toQuery"
    />
    <el-input
      v-model="query.thick"
      placeholder="输入厚度搜索"
      class="filter-item"
      style="width: 150px"
      size="small"
      clearable
      @blur="crud.toQuery"
    />
    <el-input
      v-model="query.plateType"
      placeholder="输入物料种类搜索"
      class="filter-item"
      style="width: 150px"
      size="small"
      clearable
      @blur="crud.toQuery"
    />
    <el-date-picker
      style="margin-right: 5px; width: 150px"
      v-model="date"
      type="date"
      size="small"
      class="filter-item"
      placeholder="请选择日期"
      @change="changeDate"
    />
    <rrOperation />
    <common-radio-button
      style="margin-top: 5px"
      v-model="query.plateState"
      :options="steelPlateEnum.ENUM"
      size="small"
      default
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <common-button
      :disabled="!(headSelect.length > 0)"
      style="margin-top: 5px; float: right; margin-left: 10px"
      size="mini"
      type="primary"
      @click="ContinueTaskClick"
    >
      继续任务
    </common-button>
    <!-- <common-button :disabled="!(headSelect.length > 0)" style="margin-top: 5px; float: right;" size="mini" type="danger" @click="termination">我要终止</common-button> -->
    <common-button :disabled="!(headSelect.length > 0)" style="margin-top: 5px; float: right" size="mini" type="warning" @click="suspend">
      暂停任务
    </common-button>
    <header-view v-model:visible="IssueVisible"></header-view>
  </div>
</template>

<script setup>
import headerView from './header-view.vue'
import { defineProps } from 'vue'
import { ElMessage } from 'element-plus'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { steelPlateEnum } from '@enum-ms/cutting'
import { ref } from 'vue'
import { suspendTask, continueTask } from '@/api/cutting/project-data'

const IssueVisible = ref(false)
const props = defineProps({
  headSelect: { type: Array, required: true }
})

const defaultQuery = {
  thick: undefined,
  plateState: steelPlateEnum.UNASSIGNED.V,
  plateType: undefined,
  machineName: undefined
}

const date = ref('')

const { crud, query, CRUD } = regHeader(defaultQuery)

async function ContinueTaskClick() {
  try {
    const message = await continueTask(props.headSelect)
    ElMessage({ message: message, type: 'success' })
    crud.toQuery()
  } catch (err) {
    console.log(err)
  }
}

function changeDate() {
  const a = new Date(date.value.getTime()).getFullYear()
  const b = new Date(date.value.getTime()).getMonth() + 1
  const c = new Date(date.value.getTime()).getDate()
  query.importTime = a + '-' + b + '-' + c
  crud.toQuery()
}

CRUD.HOOK.beforeResetQuery = () => {
  date.value = ''
}

// function termination() {
//   console.log('我要终止')
// }

async function suspend() {
  try {
    const message = await suspendTask(props.headSelect)
    ElMessage({ message: message, type: 'success' })
    crud.toQuery()
  } catch (err) {
    console.log(err)
  }
}

</script>

<style lang="scss" scoped>
.el-input {
  margin-right: 5px;
}
.el-date-picker {
  margin-right: 5px;
}
</style>
