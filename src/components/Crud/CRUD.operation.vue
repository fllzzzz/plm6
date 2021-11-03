<template>
  <div class="crud-opts">
    <span class="crud-opts-left">
      <!--左侧插槽-->
      <slot name="optLeft" />
      <common-button
        v-if="crud.optShow.batchAdd"
        v-permission="permission.batchAdd"
        class="filter-item"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click.stop="crud.toBatchAdd"
      >
        {{ props.addBatchText }}
      </common-button>
      <common-button
        v-if="crud.optShow.add"
        v-permission="permission.add"
        class="filter-item"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click.stop="crud.toAdd"
      >
        {{ props.addText }}
      </common-button>
      <common-button
        v-if="crud.optShow.edit"
        v-permission="permission.edit"
        class="filter-item"
        size="mini"
        type="success"
        icon="el-icon-edit"
        :disabled="crud.selections.length !== 1"
        @click.stop="crud.toEdit(crud.selections[0])"
      >
        修改
      </common-button>
      <common-button
        v-if="crud.optShow.del"
        v-permission="permission.del"
        class="filter-item"
        type="danger"
        icon="el-icon-delete"
        size="mini"
        :loading="crud.delAllLoading"
        :disabled="crud.selections.length === 0"
        @click.stop="toDelete(crud.selections)"
      >
        {{ props.delText }}
      </common-button>
      <common-button
        v-if="crud.optShow.download"
        :loading="crud.downloadLoading"
        :disabled="!crud.data.length"
        class="filter-item"
        size="mini"
        type="warning"
        icon="el-icon-download"
        @click.stop="crud.doExport"
        >导出</common-button
      >
      <!--右侧-->
      <slot name="optRight" />
    </span>
    <span class="crud-opts-right">
      <slot v-if="device !== 'mobile'" name="viewLeft" />
      <el-button-group style="max-width: 142px;margin-left: 6px;">
        <el-button
          v-if="props.showSearch"
          size="mini"
          plain
          type="info"
          icon="el-icon-search"
          :style="{ border: !props.showRefresh && !props.showGrid ? '1px solid #d3d4d6' : '' }"
          @click.stop="toggleSearch()"
        />
        <el-button v-if="props.showRefresh" size="mini" icon="el-icon-refresh" @click.stop="crud.refresh()" />
        <el-popover v-if="props.showGrid" placement="bottom-end" width="150" trigger="click">
          <template v-slot:reference>
            <el-button size="mini" icon="el-icon-s-grid">
              <!-- <i
              class="fa fa-caret-down"
              aria-hidden="true"
            /> -->
            </el-button>
          </template>
          <div class="column-list">
            <el-checkbox
              v-model="allColumnsSelected"
              :indeterminate="allColumnsSelectedIndeterminate"
              @change="handleCheckAllChange"
              size="mini"
            >
              全选
            </el-checkbox>
            <template v-for="item in crud.tableColumns">
              <el-checkbox
                v-if="typeof item === 'object'"
                :key="item.label"
                v-model="item.visible"
                size="mini"
                @change="handleCheckedTableColumnsChange(item)"
              >
                {{ item.label }}
              </el-checkbox>
            </template>
          </div>
        </el-popover>
      </el-button-group>
    </span>
  </div>
</template>
<script setup>
import { defineProps, ref, inject, nextTick } from 'vue'
import { regExtra } from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import { ElMessageBox, ElButtonGroup, ElButton } from 'element-plus'

const props = defineProps({
  showRefresh: {
    type: Boolean,
    default: true
  },
  showGrid: {
    type: Boolean,
    default: true
  },
  showSearch: {
    type: Boolean,
    default: true
  },
  addBatchText: {
    type: String,
    default: '批量新增'
  },
  addText: {
    type: String,
    default: '新增'
  },
  delText: {
    type: String,
    default: '删除'
  }
})

const permission = inject('permission')

const allColumnsSelected = ref(true)
const allColumnsSelectedIndeterminate = ref(false)

const { CRUD, crud } = regExtra()

const { device } = mapGetters('device')

// 批量删除
function toDelete(datas) {
  ElMessageBox.confirm(`确认${props.delText}选中的${datas.length}条数据?`, '提示', {
    confirmButtonText: '确定',
    cancelButtonText: '取消',
    type: 'warning'
  })
    .then(() => {
      crud.delAllLoading = true
      crud.doDelete(datas)
    })
    .catch(() => {})
}

// 列：全选
function handleCheckAllChange(val) {
  if (val === false) {
    allColumnsSelected.value = true
    return
  }
  for (const key in crud.tableColumns) {
    crud.tableColumns[key].visible = val
  }
  allColumnsSelected.value = val
  allColumnsSelectedIndeterminate.value = false
}

// 处理表格选择变更
function handleCheckedTableColumnsChange(item) {
  let totalCount = 0
  let selectedCount = 0
  for (const key in crud.tableColumns) {
    ++totalCount
    selectedCount += crud.tableColumns[key].visible ? 1 : 0
  }
  if (selectedCount === 0) {
    crud.notify('请至少选择一列', CRUD.NOTIFICATION_TYPE.WARNING)
    nextTick(function () {
      item.visible = true
    })
    return
  }
  allColumnsSelected.value = selectedCount === totalCount
  allColumnsSelectedIndeterminate.value = selectedCount !== totalCount && selectedCount !== 0
}

// toggleSearch切换
function toggleSearch() {
  crud.props.searchToggle = !crud.props.searchToggle
  nextTick(() => {
    // TODO:判断是否还需要
    // 手动resize 来触发对resize的监听方法：$_windowSizeHandler
    var e = document.createEvent('Event')
    e.initEvent('resize', true, true)
    window.dispatchEvent(e)
  })
}
</script>

<style lang="scss" scoped>
.column-list {
  display: inline-flex;
  flex-direction: column;
}
.common-button + .common-button {
  margin-left: 0;
}
.crud-opts {
  padding: 6px 0;
  display: -webkit-flex;
  display: flex;
  align-items: center;
  .crud-opts-left {
    ::v-deep(.filter-item) {
      margin-bottom: 0;
    }
  }
  ::v-deep(.crud-opts-right) {
    margin-left: auto;
    display: flex;
    .filter-item {
      margin-bottom: 0;
    }
  }
}
</style>
