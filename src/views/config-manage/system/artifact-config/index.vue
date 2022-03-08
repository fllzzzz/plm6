<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-button type="primary" @click="formVisible=true" v-permission="permission.add">添加</common-button>
    <common-table
    ref="tableRef"
    :data="tableData"
    empty-text="暂无数据"
    :max-height="maxHeight"
    style="width: 100%;margin-top:10px;"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="组立标记" min-width="150">
      <template v-slot="scope">
        <div>{{ scope.row }}</div>
      </template>
    </el-table-column>
  </common-table>
  <mForm :list="tableData" v-model="formVisible" @success="fetchData"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref } from 'vue'
import { artifactConfigPM as permission } from '@/page-permission/config'
import useMaxHeight from '@compos/use-max-height'
import mForm from './module/form'

const tableRef = ref()
const tableData = ref([])
const formVisible = ref(false)
const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifactConfig',
  paginate: true,
  extraHeight: 40
})

fetchData()

async function fetchData() {
  try {
    const { content } = await crudApi.get({ productType: 16 })
    tableData.value = content || []
  } catch (e) {
    console.log('组立标记', e)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
