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
  import { ref } from 'vue'
  import useCRUD from '@compos/use-crud'
  import useMaxHeight from '@compos/use-max-height'
  import checkPermission from '@/utils/system/check-permission'
  // import { equipmentMonitoringPM as permission } from '@/page-permission/cutting'
  import mHeader from './module/header'
  import projectChart from './project-chart'

  const optShow = {
    add: false,
    edit: false,
    del: false,
    download: false
  }
 const tableRef = ref()
 
  const { crud,column } = useCRUD(
  {
    title: '设备监控',
    sort: [],
    // permission: { ...permission },
    optShow: { ...optShow },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

  const { maxHeight } = useMaxHeight({
    paginate: true,
   
  })
</script>
<style lang="scss" scoped>
  .app-container {
    width: 1700px;
    display: flex;
    flex-direction: column;
  }    
  .app-content {
      // height: 700px;
      display:flex;

    }
    .app-content .left-content {
        width: 1000px;
        border: 1px solid #ccc;
        margin-right: 20px;
      }
          
     .app-content .right-content {
        flex: 1;  
      }
 
</style>




