<template>
  <div>设备监控</div>
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
    >
      
      </common-table> -->
      
  </div>
</template>

<script setup>
  import crudApi from '@/api/cutting/machine'
  import { ref } from 'vue'
  import useCRUD from '@compos/use-crud'
  import useMaxHeight from '@compos/use-max-height'
  import checkPermission from '@/utils/system/check-permission'
  // import mHeader from './module/header'

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
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

  // const { maxHeight } = useMaxHeight({
  //   wrapperBox: '.contractRecord',
  //   paginate: true,
  //   extraHeight: 40
  // })
</script>




