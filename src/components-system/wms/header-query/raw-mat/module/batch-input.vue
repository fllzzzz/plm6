<template>
  <div class="batch-input" :class="{ 'batch-input-edit': props.edit }">
    <el-input
      v-model.trim="queryVO.heatNoAndBatchNo"
      clearable
      style="width: 100%"
      size="small"
      :placeholder="props.placeholder"
      @keyup.enter="toQuery"
    />
    <el-icon v-if="props.edit" class="batch-input-icon" @click="show">
      <el-icon-edit />
    </el-icon>
    <common-dialog
      :title="`${props.placeholder}批量填写`"
      v-model="dialogVisible"
      width="70%"
      :before-close="handleClose"
      :show-close="true"
    >
      <template #titleRight>
        <common-button size="mini" type="primary" @click="search"> 搜 索 </common-button>
        <common-button size="mini" type="success" @click="add"> 新 增 </common-button>
        <common-button size="mini" type="danger" @click="del"> 删 除 </common-button>
      </template>
      <el-row :gutter="10" class="dialog-input-wrap">
        <el-col :span="8" v-for="(item, index) in list" :key="index">
          <el-input
            v-model.trim="list[index]"
            clearable
            :maxlength="50"
            style="width: 100%; margin-top: 10px"
            size="small"
            :placeholder="props.placeholder"
            @keyup.enter="search"
          />
        </el-col>
      </el-row>
    </common-dialog>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'

import { isNotBlank } from '@data-type/index'
import { uniqueArr } from '@/utils/data-type/array'

const emit = defineEmits(['to-query'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: true
  },
  placeholder: {
    type: String,
    default: '炉批号'
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const queryVO = ref({})
const dialogVisible = ref(false)
const list = ref([])

watchEffect(() => {
  queryVO.value = props.query
})

function toQuery() {
  emit('to-query')
}

function show() {
  list.value = uniqueArr((queryVO.value.heatNoAndBatchNo || '').split(/[,，]/))
  if (list.value.length < 9) {
    list.value.length = 9
  }
  dialogVisible.value = true
}

function handleClose() {
  dialogVisible.value = false
}

function search() {
  queryVO.value.heatNoAndBatchNo = uniqueArr(list.value.filter((val) => isNotBlank(val))).join(',')
  handleClose()
  toQuery()
}

function add() {
  list.value.length += 9
}

function del() {
  if (list.value.length > 9) {
    list.value.length -= 9
  } else {
    list.value.length = 0
  }
}
</script>

<style lang="scss" scoped>
.batch-input {
  position: relative;
  display: inline-block;
}
.batch-input-edit {
  ::v-deep(.el-input) {
    .el-input__inner {
      padding-right: 50px;
    }
    .el-input__suffix {
      right: 25px;
    }
  }
  .batch-input-icon {
    color: #c2c6ce;
    font-size: 18px;
    font-weight: bold;
    position: absolute;
    width: 30px;
    height: 100%;
    top: 0;
    right: 0;
    margin: auto;
    display: flex;
    justify-content: center;
    align-items: center;
    cursor: pointer;
    &:hover {
      color: #409eff;
    }
  }
}
.dialog-input-wrap {
  max-height: calc(70vh - 104px);
  overflow-y: auto;
}
</style>
