<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.fileName"
        placeholder="输入文件名搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          ref="changeFileRef"
          v-permission="crud.permission.import"
          :upload-fun="upload"
          :data="carryParam"
          btn-name="文件上传"
          btn-type="warning"
          btn-size="mini"
          :data-type="crud.query.type"
          icon="el-icon-upload"
          accept=".zip"
          class="filter-item"
          @success="crud.toQuery"
        />
      </template>
      <!-- <template #viewLeft>
        <common-button
          v-if="projectId"
          v-permission="permission.download"
          :loading="downloadLoading"
          type="warning"
          icon="el-icon-download"
          size="mini"
          :disabled="!projectId"
          @click.stop="downloadAll()"
        >下载项目下所有文件</common-button>
      </template> -->
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import uploadBtn from '../../components/drawing-upload-btn'
import { upload } from '@/api/plan/technical-data-manage/other'

const defaultQuery = {
  type: technicalDataTypeEnum.CHANGE_FILE.V // 类型 1蓝图 2变更文件 3模型 4其他文件
}

const monomerSelectRef = ref()
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const carryParam = computed(()=>{
  return { projectId: props.projectId, type: crud.query.type }
})
const changeFileRef = ref()
</script>
