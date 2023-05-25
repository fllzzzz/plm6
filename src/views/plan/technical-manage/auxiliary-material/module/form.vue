<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :wrapper-closable="false"
    size="90%"
    custom-class="raw-mat-inbound-application-record-form"
  >
    <template #titleAfter>
     <div>项目:<span>{{globalProject.serialNumber}}</span><span style="margin-left:5px;">{{globalProject.shortName}}</span></div>
     <div v-if="isNotBlank(currentMonomer)" style="margin-left:10px;">单体:{{currentMonomer.name}}</div>
     <div v-if="isNotBlank(currentArea)" style="margin-left:10px;">区域:{{currentArea.name}}</div>
    </template>
    <template #content>
      <component :is="comp" :detail="form" @success="handleSuccess" :project="crud.query"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, inject } from 'vue'
import { regForm } from '@compos/use-crud'
import { isNotBlank } from '@/utils/data-type'
import AuxMatApplication from './auxiliary-material-form'

const { crud, form } = regForm()
const currentMonomer = inject('currentMonomer')
const currentArea = inject('currentArea')
const globalProject = inject('globalProject')
const comp = computed(() => {
  return AuxMatApplication
})

function handleSuccess() {
  crud.cancelCU()
  crud.refresh()
}
</script>

<style lang="scss" scoped>
::v-deep(.inbound-application-container) {
  .header {
    padding: 0 0 10px 0;
  }
  .main-content {
    padding: 0;
  }
}
</style>
