<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <div class="head-container">
      <mHeader/>
    </div>
     <!-- <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 60%"
      :stripe="false"
    > </common-table> -->
    <div class="app-content">
      <div class="left-content">
      </div>
      <div class="right-content">
        <projectChart  />
      </div>
    </div>
  </div>
</template>

<script setup>
// import crudApi from '@/api/cutting/machine'
import { getMachineInformation } from '@/api/cutting/machine'
import { defineProps } from 'vue'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header'
import projectChart from './project-chart'
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const props = defineProps({
  detailData: {
    type: Object
  }
})
const { crud } = useCRUD(
  {
    title: '设备监控',
    sort: [],
    // permission: { ...permission },
    optShow: { ...optShow },
    hasPagination: true
  }
)
machineInformation()
async function machineInformation() {
  try {
    const content = await getMachineInformation(props.detailData)
    console.log(content)
  } catch (error) {
    console.log('请求设备监控的接口失败')
  }
}

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})
</script>
<style lang="scss" scoped>
  .app-content {
      display:flex;
    }
    .app-content .left-content {
        border: 1px solid #ccc;
        margin-right: 20px;
        flex: 2;
        display: flex;
        padding: 10px;
      }
    .app-content .right-content {
      display: flex;
      flex: 1;
      flex-direction: column;
    }
</style>

