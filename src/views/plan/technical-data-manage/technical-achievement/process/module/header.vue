<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="编号搜索"
        style="width: 170px"
        class="filter-item"
      />
       <el-input
        v-model="query.classifyName"
        size="small"
        placeholder="规格搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
      />
      <el-input
        v-model="query.fileName"
        placeholder="材质搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
// import { defineProps, ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
// import checkPermission from '@/utils/system/check-permission'

const defaultQuery = {
  monomerId: undefined
}

// const emit = defineEmits(['currentChange', 'handleUpload'])
const { crud, query } = regHeader(defaultQuery)

</script>
